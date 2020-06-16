# Monadception

A simple example of an approach to tagless final, where we:

1. Provide operations as usual
2. Wrap resulting programs
3. Implement Monad for the program wrappers
4. Provide the underlying tagless final operations as instances of the wrapper monad
5. Compose programs using the wrapper monad operations

This seems to give usability advantages by making the signature of programs simpler, while still retaining the ability to convert the programs to any other Monad for which an implementation of the operations is available. If necessary, the "plain" tagless final style is still available and can be used interchangeably.

The obvious disadvantages are:

1. More boilerplate - we define the operations once in tagless final style, and then again in the wrapper monad
2. More wrapping - we introduce instances of the wrapper monad as we build programs
3. May not be stack safe - need to check
4. Harder to compose - in plain tagless final we can provide an arbitrary set of operations as implicit parameters, and the compiler will check that the correct operations are in implicit scope. With the wrapper monad we need to provide explicit combinations of operations, with one wrapper monad for each combination. However when we do this, it is relatively straightforward to provide implicit conversions from any wrapper monad with fewer operations to one with more. Practically this might not be a major problem for actual DSLs. So in the example given, we have `ReadOps`, and then `EditOps` that are a strict superset of `ReadOps` (and a subclass). This means that we can provide a `Read` wrapper monad, and an `Edit` wrapper monad, with an implicit conversion allowing any instance of `Read` to be used as an instance of `Edit`. In this case the only cost is some boilerplate - for more complex, overlapping sets of operations this might be a problem.

## Todo

- [ ] Establish whether this approach is useful - are there better existing approaches?
- [ ] Implement monad law tests for the wrapper monad
- [ ] Check the wrapper monad is stack safe
- [ ] Look into efficiency of wrapper monad compared to plain tagless final
- [ ] Look at reducing or automating boilerplate
