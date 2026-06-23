# ADR 1: Instantly process effects

## Background

The Nexus execution model imposes a limitation that occasionally trips people
up. When dispatching a set of actions, Nexus:

1. Expands actions (recursively) until there is a list of effects.
2. Executes all effects.

In other words, effects are always executed after all action expansions. This
means that all action expansions see the same state snapshot, as there is no way
for state to change during a single dispatch.

As a consequence, it is not possible to implement update-style actions (e.g.
actions that base new state on current state) that can be used more than once in
the same dispatch.

## Example

Given the following Nexus system:

```clj
(require '[nexus.registry :as nxr])

(nxr/register-effect! :effects/save
  (fn [_ system path value]
    (swap! system assoc-in path value)))

(nxr/register-action! :actions/inc
  (fn [state path]
    [[:effects/save path (inc (get-in state path))]]))

(nxr/register-system->state! deref)
```

This will work as intended (e.g. the `:number` key in the `system` atom will be
increased by one):

```clj
(def system (atom {:number 0}))
(nxr/dispatch system {} [[:actions/inc [:number]]])

@system ;;=> {:number 1}
```

This will not work as intended. Both action expansions see the current state,
and perform the same increment.

```clj
(def system (atom {:number 0}))

(nxr/dispatch system {} [[:actions/inc [:number]]
                        [:actions/inc [:number]]])

@system ;;=> {:number 1}
```

During initial design and implementation, care was taken to make it possible for
a single Nexus dispatch to result in a single transactional update to the system
state, under the assumption that state updates would trigger a render. Nexus
supports batching effects to make this possible.

Batching is not the only way to ensure that a single dispatch results in a
single render. Options outside of Nexus - like tying renders to dispatch instead
of state updates, or using a render-lock during dispatch - was not thoroughly
investigated.

## Decision: Change the execution model

In this revised execution model, when dispatching a set of actions, Nexus:

1. Expands an action until it finds an effect
2. If the effect is a `^:nexus/batch` effect put it in a list, then repeat the
   process from 1.
3. If the effect is not batched, execute it, update the state snapshot and
   repeat the process until there are no more actions.
4. Execute any batched effects.

## Consequences

In a system where every effect uses `^:nexus/batch`, there will be no observable
changes from today's behavior.

Unbatched effects will be executed earlier after this change, even before any
remaining actions are expanded.

Action expansion will receive state that includes changes from previously
executed effects from the same dispatch. Previously all action expansions in the
same dispatch received the same pre-dispatch state. In other words, the
motivating example will behave differently now:

```clj
(def system (atom {:number 0}))

(nxr/dispatch system {} [[:actions/inc [:number]]
                        [:actions/inc [:number]]])

@system ;;=> {:number 2} (Previously {:number 1})
```

### Cons

Changes the behavior for state snapshots in action expansions in a way that
isn't backwards compatible.

### Pros

All existing Nexus code examples continue to mirror the suggested use of Nexus.

Action expansion will always work with an updated state snapshot.

The changed behavior does not come with new restrictions in use.

The changed behavior enables usage that isn't currently possible.

Discussions with Nexus users shows that the new behavior aligns with existing
expectations -- and that the previous behavior was considered a bug by some.

## Alternatives

### Alternative #1: Add another dispatch strategy

Implement the changed behavior as a new, separate strategy users can choose.

By providing this strategy from a new namespace (`nexus.serial` or similar),
users would be able to opt in to it, and when doing so - be able to dead code
eliminate the old strategy.

`nexus.registry` implicitly uses the batch strategy. In order to be able to dead
code eliminate full namespaces, the registry would also need to mirror the
strategy. Since another registry would be required, the public API should
probably be revamped to:

- `nexus.batch.dispatch` - The existing batch strategy
- `nexus.batch.registry` - The existing registry using batching
- `nexus.serial.dispatch` - The new strategy
- `nexus.serial.registry` - A registry using the new strategy

And then for backwards compatibility:

- `nexus.core` - Alias of `nexus.batch.dispatch`
- `nexus.registry` - Alias of `nexus.batch.registry`

Note: Duplicating the existing namespaces isn't strictly necessary, but would
make it easier to talk about these as two different strategies.

### Pros

Fully backwards compatible. Explicit and opt-in.

### Cons

This solution effectively doubles Nexus' surface area, without really providing
the user with two meaningfully different options. If you don't have any
`^:nexus/batch` effects, this is essentially a choice of whether you want your
action expansion state snapshots to be stale or fresh.

All existing code samples (videos, code using Nexus, articles, etc) will use an
API that is no longer encouraged.

This solution leaves Nexus without a clear entry-point (you'll need to choose
between two strategies), and `nexus.core` would be deprecated.

## Alternative #2: Just make a new thing!

After reviewing the differences between the two strategies, it's become clear
that the batching feature currently implemented in `nexus.core` makes a bad
trade-off, and we didn't do a good enough job in considering alternatives (such
as render locking) to it.

Historically it has been my position in situations like this to leave the
existing project alone, and make a new thing that doesn't have the problems
you've identified. So one solution could be to change the current execution
strategy to allow for updated state snapshots, and release it as a new library.

### Pros

Does not break backwards compatibility for Nexus users.

Provides a clean interface for new users that removes features found to make
poor trade-offs.

### Cons

While this solution doesn't break existing code examples, it still invalidates
them by effectively declaring Nexus dead.

This new library isn't really new. It still delivers on the same basic premise.
Two libraries solving the exact same problem with only minute differences isn't
helping anyone.

Many developers already know Nexus, restarting from scratch loses all built up
momentum.

## Alternative #3: Release a Nexus 2.0

This is the soft version of solution #2, while also being similar to solution
#1. It shares cons with both options, and is arguably not really
backwards-compatible either.

----

Breaking changes are broken, but all told, a breaking change seems like the
least disruptive way forward.
