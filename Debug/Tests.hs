module Tests where
import Data.List (sort)
import Utils
import Genetic
import Simulation
import Neural
import PID
import System
import System.IO
import Test.QuickCheck

------------------------------------------------------------------------
-- Utils Tests
-- Coverage excludes: penult, neck, random*,
------------------------------------------------------------------------

propGroupsInv (n, xs) = concat (groups n xs) == xs

propAvgIdem x = x `avg` x == x

propAverageUpperLimit xs = not (null xs) ==> average xs < (head . sort) xs
propAverageLowerLimit xs = not (null xs) ==> average xs > (last . sort) xs

propKeySortIdem x = not (null x) ==> ksort id (ksort id x) == x

propKeySortSort x = not (null x) ==> ksort id x == sort x

propCompInv (n, xs) = (n >= 0 && n < length xs) ==> decomp (comp xs n) == xs

propDecompLen (xs, y, zs) = decomp (xs, y, zs) == (y:xs ++ zs)

geneticTest :: IO ()
geneticTest = do
        let iters = 10000
        let ffunc x = (head x + 5)**2
        let (sweight, mweight, cweight) = (0.05, 0, 0.7)
        let (popsize, gpc, grange) = (15, 1, (-8, -2))
        let gcfg = GConfig ffunc sweight mweight cweight popsize gpc grange 5
        outchrom <- runGen iters gcfg
        putStr "This should be about -5: "
        print outchrom

utilTest :: IO ()
utilTest = do
        putStrLn (if last [0, 0, 0, 1] == 1 then "Last works!" else "Last doesn't work.")
        putStrLn (if penult [0, 0, 1, 0] == 1 then "Penult works!" else "Penult doesn't work.")
        putStrLn (if neck [0, 1, 0, 0] == 1 then "Neck works!" else "Neck doesn't work.")
        putStr "This should be a random integer: "
        x <- randomInt (0, 1)
        print x
        putStr "This should be a random floating-point number: "
        y <- randomDouble (0, 1)
        print y
        a <- randomIntList 100 (0, 1)
        let b = average (map fromIntegral a)
        putStr "This should be about 0.5: "
        print b
        c <- randomDoubleList 100 (0, 1)
        let d = average c
        putStr "This should be about 0.5: "
        print d
        let e = (zip [9,8..1] (replicate 9 0))
        putStrLn (if ksort fst e == reverse e then "Sort works!" else "Sort doesn't work.")
        putStrLn (if avg 1 0 == 0.5 then "Avg works!" else "Avg doesn't work.")

simulTest :: IO ()
simulTest = do
        let scfg' = (0.1, 1, 1, 2, 10, protoState) -- step, m1, m2, r, g
        let scfg = protoConfig scfg'
        let start = SState 0 0 0.1 0 0 0
        let change (SState t x theta v w f) = SState t x theta v w (sin t)
        let test' = map stateTuple (take 2000 (iterate (update scfg . change) start))
        let test = show test'
        let script1 = "set time\nset terminal png\nset xr [0:20]\nset yr [-pi:pi]\nset output \"./TEST.png\"\n"
        let script2 = "plot \"./temp.csv\" using 1:2 title \"x\" with line, \\\n"
        let script3 = "\"./temp.csv\" using 1:3 title \"theta\" with line, \\\n"
        let script4 = "\"./temp.csv\" using 1:4 title \"v\" with line, \\\n"
        let script5 = "\"./temp.csv\" using 1:5 title \"w\" with line\n"
        let script = script1 ++ script2 ++ script3 ++ script4 ++ script5
        scriptHandle <- openFile "./script" WriteMode
        hPutStr scriptHandle script
        hClose scriptHandle
        csvHandle <- openFile "./temp1.csv" WriteMode
        hPutStr csvHandle test
        hClose scriptHandle
        command "sed -i 's/),(/\\n/g' ./temp1.csv"
        command "sed -i 's/,/ /g' ./temp1.csv"
        command "sed -i 's/\\[(//g' ./temp1.csv"
        command "grep -v 'NaN' ./temp1.csv > ./temp.csv"
        command "gnuplot script"
        command "rm ./*.csv ./script"
        putStrLn "Worked."

pidTest = do
        let iters = 10
        let efunc x y = sin x
        let ifunc = id
        let (iWeight, dWeight, pWeight) = (0.56, 2, 0.1)
        let pcfg = PConfig iWeight dWeight pWeight 0.1 efunc ifunc 0.5
        let error = runPID iters pcfg
        errorHandle <- openFile "./error.dat" WriteMode
        hPutStr errorHandle (show error)
        hClose errorHandle
        putStrLn "Worked."
