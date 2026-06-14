# Batching effects

Warning! This feature is deprecated and discouraged because it can reorder
effects and cause confusion. It was initially offered as a way to ensure one
dispatch only resulted in one render, but there are better solutions to this
problem. See [the relevant section of the Readme](Readme.md#rendering).

----

In the Readme example, `:task/edit` expands into multiple `:effects/save`
effects. With the presented implementation, this will cause several calls to
`swap!`. If you want action dispatch to be atomic, you can _batch_
`:effects/save`. To do this, install the batching interceptor, mark the handler
function with `:nexus/batch` meta data, and change its signature. It will now
receive a collection of effect arguments

```clj
(require [nexus.batching :as batching])

(def nexus
  (-> {,,,
       :nexus/effects
       {:effects/save
        ^:nexus/batch
        (fn [_ctx system path-vs]
          (swap! system
                 (fn [state]
                   (reduce (fn [acc [path v]]
                             (assoc-in acc path v))
                           state path-vs))))}}
      batching/install))
```

Now every `:effects/save` will be handled together, resulting in only a single
`swap!`.

Batching effects may cause them to be executed out of order. Nexus must expand
all actions with action handlers before any batched effect can be executed.
