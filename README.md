# alt-test

## Description
Library exports function `void* compareBranches(char* a1, char* a2)` that returns pointer to string, containing JSON reslut of compairing two branches.

## Dependencies
* zlib-devel
* ghc8.6.4
* ghc8.6.4-aeson
* ghc8.6.4-http-client
* ghc8.6.4-http-client-tls

## Building

To build using `stack`, just run `stack install`. It will create a `.so` file under `.stack-work` and install it for futher linking.

Since I can't install `stack` on Alt Linux properly, here is an example of building with `ghc`:
* To further simplify working with paths:
```
    export GHC_LIBDIR=$(ghc --print-libdir)
```

* Make a directory for lib:
```
mkdir $GHC_LIBDIR/alt-test-0.1.0.0
```
* Compile sources in the project's dir:
```
ghc -O2 -shared -dynamic -fPIC -o $GHC_LIBDIR/alt-test-0.1.0.0/libHSalt-test-0.1.0.0-ghc8.6.4.so src/Net.hs src/EVRComparison.hs src/AltBranches.hs -lHSrts-ghc8.6.4
```
* make symbolic link for shared library:
```
ln -s $GHC_LIBDIR/alt-test-0.1.0.0/libHSalt-test-0.1.0.0-ghc8.6.4.so $GHC_LIBDIR/lib/libHSalt-test-0.1.0.0-ghc8.6.4.so
```

* Update ldconfig cache:
```
ldcondig
```

## Example

To compile and successfully link an example from `example/main.c`, you need to add two libraries and a header (for `hs_init` and `hs_exit` – `HSrts-ghc8.6.4`, for `compairingBranches` - `HSalt-test-0.1.0.0-ghc8.6.4`, for `HHsFFI.h` - `$GHC_LIBDIR/include/`) to linker:
```
gcc example/main.c -L$GHC_LIBDIR/rts/ -L$GHC_LIBDIR/alt-test-0.1.0.0/ -lHSrts-ghc8.6.4 -lHSalt-test-0.1.0.0-ghc8.6.4 -I$GHC_LIBDIR/include
```
