# Senex

Uses Astrodientst's [Swiss Ephemeris](https://www.astro.com/swisseph/swephinfo_e.htm) C library in Haskell, exposing it as a Module and then using it for a Servant-based API.

## Haskell bindings to the SwissEphemeris library

### Compiling the C libs

(TODO: maybe should add an `install` task  to the C bits)

```
➜  csrc git:(master) ✗ make libswe.so
cc	 -c -g -Wall -fPIC  	   swedate.c     
cc	 -c -g -Wall -fPIC  	   swehouse.c     
cc	 -c -g -Wall -fPIC  	   swejpl.c     
cc	 -c -g -Wall -fPIC  	   swemmoon.c     
cc	 -c -g -Wall -fPIC  	   swemplan.c     
cc	 -c -g -Wall -fPIC  	   swepcalc.c     
cc	 -c -g -Wall -fPIC  	   sweph.c     
cc	 -c -g -Wall -fPIC  	   swepdate.c     
cc	 -c -g -Wall -fPIC  	   swephlib.c     
cc	 -c -g -Wall -fPIC  	   swecl.c     
cc	 -c -g -Wall -fPIC  	   swehel.c     
cc	 -shared -o libswe.so swedate.o swehouse.o swejpl.o swemmoon.o swemplan.o swepcalc.o sweph.o swepdate.o swephlib.o swecl.o swehel.o
➜  csrc git:(master) ✗ make libswe.a
ar r libswe.a	swedate.o swehouse.o swejpl.o swemmoon.o swemplan.o swepcalc.o sweph.o swepdate.o swephlib.o swecl.o swehel.o
ar: creating archive libswe.a
➜  csrc git:(master) ✗ make libswe.dylib

➜  csrc git:(master) ✗ cp libswe.* /usr/local/lib/

## Gotta export the nonstandard location I'm putting things in:

export LD_LIBRARY_PATH="/usr/local/lib"
```

N.B. I _think_ that by adding the `extra-lib-dirs` setting we may not need that?

### Compiling the `hsc` files

Run `hsc2hs src/*.hsc` to produce actual Haskell files. Stack seems to happily ignore hsc files? Although maybe adding `SWE` and `Foreign.SWE` to the exposed modules may fix it?

## Using the C Bindings

Example main (based on https://www.astro.com/swisseph/swephprg.htm#_Toc19111155):

```
  setEphemeridesPath "/Users/luis/code/senex/csrc/sweph_18"
  let time = julianDay 1989 1 6 0.0
  let coords = map (\p -> (p, (calculateCoordinates time p))) [Sun .. Chiron]
  forM_ coords $ \(planet, coord)->
    putStrLn $ show planet ++ ": " ++ show coord
```