# Backlog mechanism census

A classification of every filed `TODO.md` item by the **mechanism that produced it** — the reason
the defect could exist at all — rather than by the subsystem it lives in.

It exists because the backlog does not converge. New items arrive every round, and grouping by area
could not show whether they were independent mistakes or instances of a smaller number of recurring
design decisions. This answers that: **~62% of the backlog is generator-produced, ~22% is not.**

## Files

- **`mechanism-codebook.md`** — the controlled vocabulary. 19 terms, converged (no new term was
  needed after ~item 120, and the smallest still holds 8 items). This is the durable part: it is the
  scheme new items get classified against.
- **`mechanism-classification-2026-08-23.txt`** — one line per item: `n · primary · secondary · the
  item's stated mechanism`. **Transitional data**, kept to back-fill the `mechanism` field when the
  backlog moves to a `todo/` directory. Delete it once that back-fill has landed.

## What it found

- Top 5 mechanisms cover **52.6%** of 637 items; top 10 cover **76.6%**.
- The top three — lane replication, unchoked sibling sites, name/shape-matched semantics — are
  **243 items (38.1%) spanning all ten areas**, and are three faces of one thing: *a decision that
  should have one source of truth is spelled in more than one place.*
- **Area grouping hides this.** Every one of the top eight mechanisms has the MAJORITY of its members
  outside its own largest area (77%, 74%, 73%, 73%, 72%, 68%, 61%, 51%).
- The generators are **still firing**: the mechanism mix of the newest filings matches the mix of the
  whole backlog, so nothing is being worked off faster than it is minted.
- The project already has this vocabulary and applies it **about a third of the time** — `Core #9` is
  named in 45 entries and describes 134; `Core #4` named in 27, describes 56; `Core #13` named in 7,
  describes 57.
- **~22% has no generator at all** — a ratified-design queue, open design questions, doc↔code drift,
  harness limits, unbuilt scope, record hygiene. No architectural fix touches those, and saying so is
  as useful as the confirmed part.

## How much to trust it

Two controls were run, and the honest one is the second.

- A deliberately reasonable **keyword classifier scored 32.3% agreement** against the hand census,
  failing by over-prediction from incidental vocabulary (`doc↔code` over-predicted 4.0×, `dead guard`
  3.7×). Term frequency cannot classify this corpus, because the entries carry evidence and
  refutations that mention every keyword. **The census does not rest on it.**
- An **independent second rater**, given only the codebook and 60 randomly sampled items with no
  access to the first answers, agreed **78.3% exactly (Cohen's κ = 0.757)**, 86.7% counting
  secondaries. It independently flagged the same three soft boundaries — which makes those codebook
  defects rather than rater noise, and they are named in the codebook.

## Known limits

- **73 bullets are composites** holding 327 sub-bullets; they are classified by DOMINANT mechanism,
  not decomposed. The true count of discrete findings is **~891, not 637**.
- Primary-vs-secondary is a judgement call on ~150 items. That is why the superfamily grouping is
  reported: it is invariant under the choice.
- It classifies **what the backlog says, not whether each item still reproduces.** An unknown
  fraction is stale — three entries are themselves records of previously-filed claims being refuted.
  A stale-scan is a different track.
