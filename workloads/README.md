# Workloads

Note: stack seems to run with limited permissions, so it's unable to create a stream of fs events, so don't use 'stack exec workloads-exe', run 'workloads-exe' directly!

So it seems (possibly due to "temporal event coalescing") that we can't always trust which event handler is called is the "right" one: when adding or deleting a file, sometimes only a 'modify' event is received.
