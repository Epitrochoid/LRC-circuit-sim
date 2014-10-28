{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when)
import Control.Exception (try, SomeException)

import Eulerdefs (lrcCircuit)
import CsvParser

import qualified Graphics.Rendering.Chart.Easy as C
import Graphics.Rendering.Chart.Backend.Cairo as B


data Options = Options {
             resistance :: Double,
             capacitance :: Double,
             inductance :: Double,
             initial_charge :: Double,
             dt :: Double,
             output_file :: String,
             csv_file :: String
             } deriving (Data, Typeable, Show, Eq)

optSettings :: Options
optSettings = Options {
                      resistance = def &= help "Resistance of the LRC circuit",
                      capacitance = def &= name "c" &= help "Capacitance of the LRC circuit",
                      inductance = def &= name "l" &= help "Inductance of the LRC circuit",
                      initial_charge = def &= name "q" &= help "Initial charge of the LRC circuit",
                      dt = def &= help "Time increment for ODE solution",
                      output_file = def &= help "Filepath for output graph",
                      csv_file = def &= help "Filepath for CSV to plot alongside solution"
                      }

getOpts :: IO Options
getOpts = cmdArgs $ optSettings
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME = "LRC Circuit Simulation"
_PROGRAM_ABOUT = "Simulates an LRC circuit using Euler's method and plots the simulation." ++
    "Numerical values may be entered in exponential notation, e.g. '10e-6'." ++
    "Output file must be .png, .svg, or .ps"

-- Entry point for the program
-- Main gets the arguments and if a csv filepath
--   is given it opens that file and passes the 
--   contents to the csv parser. The csvpoints
--   definition could be much cleaner using
--   monad transformers, but since it is only
--   done once a double case expression was quicker.
main :: IO ()
main = do
    args <- getArgs
    opts <- (if null args then withArgs ["--help"] else id) getOpts
    csvdata <- try $ readFile $ csv_file opts :: IO (Either SomeException String)
    csvpoints <- return $ case csvdata of
                    Right csvin -> case innereither csvin opts of
                                       Right points -> points
                                       Left _ -> []
                    Left _ -> []
    optionHandler opts csvpoints
    where
        innereither csvin opts = listsToPairs $ csvToDoubles csvin (csv_file opts)

optionHandler :: Options -> [(Double, Double)] -> IO ()
optionHandler opts@Options{..} csvpoints = do -- RecordWildCards fills in with 'resistance=resistance', etc
    when (resistance == 0.0) $ putStrLn "Must enter a value for resistance." >> exitWith (ExitFailure 1)
    when (capacitance == 0.0) $ putStrLn "Must enter a value for capacitance." >> exitWith (ExitFailure 1)
    when (initial_charge == 0.0) $ putStrLn "Must enter an initial charge." >> exitWith (ExitFailure 1)
    when (inductance == 0.0) $ putStrLn "Must enter a value for inductance." >> exitWith (ExitFailure 1)
    when (dt == 0.0) $ putStrLn "Must enter a time increment dt." >> exitWith (ExitFailure 1)
    when (resistance < 0) $ putStrLn "Resistance must not be negative." >> exitWith (ExitFailure 1)
    when (capacitance < 0) $ putStrLn "Capacitance must not be negative." >> exitWith (ExitFailure 1)
    when (inductance < 0) $ putStrLn "Inductance must not be negative." >> exitWith (ExitFailure 1)
    when (dt < 0) $ putStrLn "Time increment must not be negative." >> exitWith (ExitFailure 1)
    when (null output_file) $ putStrLn "Must include an output file location." >> exitWith (ExitFailure 1)
    exec opts csvpoints

-- Handles the output of the graph
-- If no csv file is given it only runs and graphs
--   the simulation, otherwise it also plots the 
--   datapoints from the csv.
exec :: Options -> [(Double, Double)] -> IO ()
exec opts@Options{..} csvpoints = B.toFile C.def output_file $ do
    C.layout_title C..= "Voltage vs Time"
    C.layout_x_axis . C.laxis_title C..= "Time, (s)"
    C.layout_y_axis . C.laxis_title C..= "Voltage, (V)"
    C.plot $ C.line "LRC Circuit" [lrcPlot]
    when (not $ null csvpoints) $ C.plot $ C.points "CSV Data" csvpoints
    where
        lrcVolts = filter (not . isNaN) $ lrcCircuit resistance capacitance inductance initial_charge 0.0 dt
        lrcPlot = zip (map (*dt) [1..800]) lrcVolts

