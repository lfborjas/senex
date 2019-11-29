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

```haskell
main :: IO
main = do 
  setEphemeridesPath "/Users/luis/code/senex/csrc/sweph_18"
  let time = julianDay 1989 1 6 0.0
  let coords = map (\p -> (p, (calculateCoordinates time p))) [Sun .. Chiron]
  let cusps  = calculateCusps time (basicCoords (14.0839053, -87.2750137)) Placidus
  forM_ coords $ \(planet, coord)->
    putStrLn $ show planet ++ ": " ++ show coord
  putStrLn $ "Cusps: " ++ show cusps
```

Which outputs:

```haskell
Sun: Right (Coords {lat = 285.64724200024165, long = -8.254238068673002e-5, distance = 0.983344884137739, longSpeed = 1.0196526213625938, latSpeed = 1.4968387810319695e-5, distSpeed = 1.734078975098347e-5})
Moon: Right (Coords {lat = 262.48117294528356, long = -4.905353383440304, distance = 2.541059357627873e-3, longSpeed = 13.539284298718682, latSpeed = 0.3392091965109866, distSpeed = -3.335582471922629e-5})
Mercury: Right (Coords {lat = 304.31440617188355, long = -1.3440800529425412, distance = 1.0631760110344726, longSpeed = 1.2740014806904785, latSpeed = 0.15124350747576998, distSpeed = -2.4544384076849012e-2})
Venus: Right (Coords {lat = 264.04869233523164, long = 0.611405334065098, distance = 1.541582583770699, longSpeed = 1.2513022022944864, latSpeed = -4.242629234528706e-2, distSpeed = 3.7838225239450584e-3})
Mars: Right (Coords {lat = 22.78483261142441, long = 0.6472654073363234, distance = 1.0224116051096686, longSpeed = 0.5238430038342773, latSpeed = 1.942622907667852e-2, distSpeed = 9.240031315557963e-3})
Jupiter: Right (Coords {lat = 56.44155800097809, long = -0.8785523476115656, distance = 4.335644198505431, longSpeed = -4.88842084035685e-2, latSpeed = 4.367591705532038e-3, distSpeed = 1.235220405072288e-2})
Saturn: Right (Coords {lat = 276.1820087613432, long = 0.7124661106433279, distance = 11.011939358359793, longSpeed = 0.11736930636883483, latSpeed = -9.50953203038811e-4, distSpeed = -2.7734883523371427e-3})
Uranus: Right (Coords {lat = 272.0517155140088, long = -0.22004079326191983, distance = 20.269982255822082, longSpeed = 5.9199702638179044e-2, latSpeed = -1.8348838403496113e-4, distSpeed = -3.835650269219664e-3})
Neptune: Right (Coords {lat = 280.1110438430756, long = 0.9024310257851641, distance = 31.197894367235328, longSpeed = 3.7822803787784465e-2, latSpeed = -1.0376379525413769e-4, distSpeed = -1.6316454570516235e-3})
Pluto: Right (Coords {lat = 224.68172926588147, long = 15.629616292066745, distance = 30.10503346247479, longSpeed = 2.3906567127179156e-2, latSpeed = 7.006203197180026e-3, distSpeed = -1.4619215232707367e-2})
MeanNode: Right (Coords {lat = 337.5234813280144, long = 0.0, distance = 2.5695552897999894e-3, longSpeed = -5.29028695601468e-2, latSpeed = 0.0, distSpeed = 0.0})
TrueNode: Right (Coords {lat = 336.0939772852687, long = 0.0, distance = 2.431002861595713e-3, longSpeed = -0.15458670098362634, latSpeed = 0.0, distSpeed = 9.461379851655086e-6})
MeanApog: Right (Coords {lat = 176.27796853267614, long = -1.658313456511187, distance = 2.7106251317225464e-3, longSpeed = 0.11092946668995039, latSpeed = -1.3957593707984951e-2, distSpeed = 0.0})
OscuApog: Right (Coords {lat = 160.76672907240018, long = -0.4175464087960888, distance = 2.7280104068905034e-3, longSpeed = -3.219056080555324, latSpeed = 0.2728854201125149, distSpeed = 4.0357015237500586e-6})
Earth: Right (Coords {lat = 0.0, long = 0.0, distance = 0.0, longSpeed = 0.0, latSpeed = 0.0, distSpeed = 0.0})
Chiron: Right (Coords {lat = 93.53727572747667, long = -6.849325566420532, distance = 11.045971701732345, longSpeed = -6.391339610156536e-2, latSpeed = 8.213606290819226e-4, distSpeed = 1.6210560093203594e-3})
Cusps: (Cusps {i = 112.20189657163523, ii = 138.4658382335878, iii = 167.69682489058204, iv = 199.79861981778183, v = 232.2797046698429, vi = 263.0249102802477, vii = 292.20189657163525, viii = 318.46583823358776, ix = 347.69682489058204, x = 19.798619817781823, xi = 52.27970466984291, xii = 83.02491028024768},Angles {ascendant = 112.20189657163523, mc = 19.798619817781823, armc = 18.277351820745423, vertex = 216.1872418365295, equatorialAscendant = 106.85773516967282, coAscendantKoch = 101.19442735316477, coAscendantMunkasey = 153.1221838791593, polarAscendant = 281.19442735316477})
```

Note that the `calculateCoordinates` function is currently using the default calculations from Swiss Ephemeris, which return the _ecliptic_ degree numbers for position. One could also obtain equatorial. To compare the output of the above main to the <astro.com> raw data, you can use this page: https://www.astro.com/swisseph/swetest.htm