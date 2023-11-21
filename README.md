# Cardano API memory leak/growth example programs

This repository contains two example programs that demonstrate that Cardano API is leaking memory
when interacting via the client2node protocol with a (local) Cardano node .

As example-interaction and to keep things comparable, both programs query the mempool capacity and
log it to the console. Both programs do this within a `forever` loop, however there are subtle
differences in both implementations, which in the `Leaky` case lead to an ever-growing memory
size of the application; where the `NonLeaky` case does not and tops out at a moderate memory
allocation.

Requirements to build and run the examples:

- GHC 9.2.5
- Cabal 3.6+
- A socket to some running cardano node

Specs of the machine that was used to test it:

- CPU: 12th Gen Intel® Core™ i9-12900K × 24
- Memory: 32.0 GiB DDR5
- OS: Ubuntu 22.04

## Leaky

This application uses the Cardano API function `queryTxMonitoringLocal` to query the information
from the mempool.
Usage: `cardano run -- cardano-api-leaky NETWORK_ID SOCKET_PATH`

## NonLeaky

This appliation is using the local node client protocol that is also used by Cardano API, to do the
same as the `Leaky` abstraction however with the essential difference that it does call
`connectToLocalNode` only once and then re-uses the `localTxMonitoringClient` to make calls.

Usage: `cardano run -- cardano-api-nonleaky NETWORK_ID SOCKET_PATH`

## Memory/Performance Analysis

- Leaky: the application memory grows unbounded reaching 680MiB after 10 minutes (application was terminated manually then) according to Ubuntu 22.04 System Monitor.

- NonLeaky: the application memory consumption tops out at ~131MiB on local machine according to the Ubuntu 22.04 System Monitor.

When running `Leaky` with `profiling: True` and `"-with-rtsopts=-N -T -s -i1.0 -hc -l-agu -p"` then we get the following memory consumptions via `eventlog2html`: TODO: this was not able to run because
when compiling with profiling on then the folloing ghc panic happened:

```
[41 of 52] Compiling Ouroboros.Consensus.Shelley.Node.TPraos ( src/shelley/Ouroboros/Consensus/Shelley/Node/TPraos.hs, dist/build/Ouroboros/Consensus/Shelley/Node/TPraos.p_o )
ghc: panic! (the 'impossible' happened)
  (GHC version 9.2.5:
        expectOnly: cgLit:Rubbish
```
