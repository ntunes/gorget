# std.ecs — Entity Component System

A reusable ECS library providing entity ID management and generic sparse-set
component storage.

## Import

```gorget
from std.ecs import EntityPool, SparseSet
```

## EntityPool

Manages entity IDs with free-list recycling.

```gorget
struct EntityPool:
    int next_id
    Vector[int] free_ids
```

### Methods

| Signature | Description |
|---|---|
| `int create(&self)` | Allocate a new entity ID (reuses freed IDs first) |
| `void destroy(&self, int id)` | Return an ID to the free list |
| `int count(self)` | Number of currently live entities |

### Usage

```gorget
from std.collections import Vector
from std.ecs import EntityPool

EntityPool pool = EntityPool(0, Vector[int]())
int e1 = pool.create()   # 0
int e2 = pool.create()   # 1
pool.destroy(e1)
int e3 = pool.create()   # 0 (recycled)
```

## SparseSet[T]

O(1) insert, remove, and lookup component storage indexed by entity ID.
Internally uses a sparse-to-dense indirection array and contiguous data arrays,
giving cache-friendly iteration and constant-time random access.

```gorget
struct SparseSet[T]:
    Vector[int] sparse
    Vector[int] entity_ids
    Vector[T] data
    int count
```

### Methods

| Signature | Description |
|---|---|
| `void insert(&self, int id, T value)` | Add or update a component for entity `id` |
| `void remove(&self, int id)` | Remove the component (swap-and-pop) |
| `bool has(self, int id)` | Check if entity `id` has a component |
| `T get(self, int id)` | Get the component value for entity `id` |
| `void set(&self, int id, T value)` | Update in place (entity must already exist) |
| `int len(self)` | Number of stored components |
| `int entity_at(self, int idx)` | Entity ID at dense index `idx` |
| `T data_at(self, int idx)` | Component value at dense index `idx` |

### Usage

```gorget
from std.collections import Vector
from std.ecs import EntityPool, SparseSet

struct Health:
    int hp
    int max_hp

EntityPool pool = EntityPool(0, Vector[int]())
SparseSet[Health] health = SparseSet[Health](Vector[int](), Vector[int](), Vector[Health](), 0)

int id = pool.create()
health.insert(id, Health(100, 100))

Health h = health.get(id)
print("{h.hp}")           # 100

health.set(id, Health(80, 100))

for i in 0..health.len():
    int eid = health.entity_at(i)
    Health val = health.data_at(i)
    print("Entity {eid}: {val.hp} HP")
```

## Building a World

Compose an EntityPool with multiple SparseSet stores to form a game world:

```gorget
from std.ecs import EntityPool, SparseSet

struct World:
    EntityPool entities
    SparseSet[Position] positions
    SparseSet[Health] health

equip World:
    int spawn(&self, int x, int y, int hp):
        int id = self.entities.create()
        self.positions.insert(id, Position(x, y))
        self.health.insert(id, Health(hp, hp))
        return id
```

See `examples/ecs/` for a full battle simulation and `examples/breakout/` for
a graphical game using std.ecs with std.gfx.
