# xtd.ecs — Entity Component System

A reusable ECS library providing generational entity ID management and generic
sparse-set component storage.

## Import

```gorget
from xtd.ecs import Entity, EntityPool, SparseSet
```

## Entity

An opaque handle carrying both an entity ID and a generation counter. Stale
handles (created before a recycle) are detected by `EntityPool.is_alive()`.

```gorget
struct Entity:
    int id
    int gen
```

`Entity` implements `Displayable`, formatting as `Entity(id:gen)`.

## EntityPool

Manages entity handles with free-list recycling and generation tracking.
Bumping the generation counter on recycle prevents the ABA problem: old handles
referring to a recycled slot are rejected rather than silently aliasing the new
occupant.

```gorget
struct EntityPool:
    int next_id
    Vector[int] free_ids
    Vector[int] generations
    Vector[bool] alive
```

### Methods

| Signature | Description |
|---|---|
| `EntityPool new()` | Create a new empty pool (static factory) |
| `Entity create(&self)` | Allocate a new entity handle (reuses freed slots first) |
| `void destroy(&self, Entity e)` | Return a slot to the free list; ignores out-of-range, already-dead, or stale handles |
| `bool is_alive(self, Entity e)` | `true` iff the handle is current (alive and generation matches) |
| `int count(self)` | Number of currently live entities |
| `int max_id(self)` | Upper bound on entity IDs (one past the highest ever allocated) |

### Usage

```gorget
from xtd.ecs import Entity, EntityPool

EntityPool pool = EntityPool.new()
Entity e1 = pool.create()   # Entity(0:0)
Entity e2 = pool.create()   # Entity(1:0)
pool.destroy(e1)
Entity e3 = pool.create()   # Entity(0:1) — id reused, generation bumped
print(pool.max_id())        # 2
print(pool.is_alive(e1))    # false — stale handle
print(pool.is_alive(e3))    # true
pool.destroy(e2)
pool.destroy(e2)            # second destroy is a no-op
print(pool.count())         # 1
```

## SparseSet[T]

O(1) insert, remove, and lookup component storage indexed by `Entity` handle.
Internally uses a sparse-to-dense indirection array and contiguous data arrays,
giving cache-friendly iteration and constant-time random access.

```gorget
struct SparseSet[T]:
    Vector[int] sparse
    Vector[Entity] entity_ids
    Vector[T] data
    int count
```

### Methods

| Signature | Description |
|---|---|
| `SparseSet[T] new()` | Static factory — create an empty set (no args needed) |
| `void insert(&self, Entity e, T value)` | Add or update a component for entity `e` |
| `void remove(&self, Entity e)` | Remove the component (swap-and-pop) |
| `bool has(self, Entity e)` | Check if entity `e` has a component |
| `T get(self, Entity e)` | Get the component value (panics if missing — use `has()` or `try_get()` first) |
| `Option[T] try_get(self, Entity e)` | Safe get — returns `None` if entity is missing |
| `void set(&self, Entity e, T value)` | Update in place (silently no-ops if entity missing) |
| `int len(self)` | Number of stored components |
| `Entity entity_at(self, int idx)` | Entity handle at dense index `idx` |
| `T data_at(self, int idx)` | Component value at dense index `idx` |
| `void each(&self, Callable[void(Entity, T)] fn)` | Iterate all (entity, component) pairs with a callback |

### Iteration

`SparseSet[T]` implements `Iterable[Entity]`, so you can iterate entity handles
directly:

```gorget
for e in health:
    Health h = health.get(e)
    print(f"{e.id}: {h.hp} HP")
```

### Construction

```gorget
from xtd.ecs import Entity, EntityPool, SparseSet

SparseSet[Health] health = SparseSet[Health].new()
```

### Usage

```gorget
from xtd.ecs import Entity, EntityPool, SparseSet

struct Health:
    int hp
    int max_hp

EntityPool pool = EntityPool.new()
SparseSet[Health] health = SparseSet[Health].new()

Entity e = pool.create()
health.insert(e, Health(100, 100))

Health h = health.get(e)
print(f"{h.hp}")           # 100

health.set(e, Health(80, 100))

for e in health:
    Health val = health.get(e)
    print(f"Entity {e.id}: {val.hp} HP")
```

## Free Functions

### `query2[A, B]` — multi-component intersection

```gorget
Vector[Entity] query2[A, B](SparseSet[A] store_a, SparseSet[B] store_b)
```

Returns the `Vector[Entity]` of entities present in **both** sparse sets. Iterates
the smaller set and checks membership in the larger, so the cost is
`O(min(|A|, |B|))`.

```gorget
from xtd.ecs import Entity, EntityPool, SparseSet, query2

Vector[Entity] movers = query2[Position, Velocity](world.positions, world.velocities)
int i = 0
while i < movers.len():
    Entity e = movers.get(i).unwrap()
    Position p = world.positions.get(e)
    Velocity v = world.velocities.get(e)
    world.positions.set(e, Position(p.x + v.dx, p.y + v.dy))
    i = i + 1
```

## Building a World

Compose an EntityPool with multiple SparseSet stores to form a game world:

```gorget
from xtd.ecs import Entity, EntityPool, SparseSet

struct World:
    EntityPool entities
    SparseSet[Position] positions
    SparseSet[Health] health

equip World:
    Entity spawn(&self, int x, int y, int hp):
        Entity e = self.entities.create()
        self.positions.insert(e, Position(x, y))
        self.health.insert(e, Health(hp, hp))
        return e
```

See `examples/ecs/` for a full battle simulation and `examples/breakout/` for
a graphical game using xtd.ecs with xtd.gfx.
