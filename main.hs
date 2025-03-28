{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

import Graphics.UI.Gtk
import Numeric.LinearAlgebra hiding (Vector, Matrix, (<.>))
import qualified Numeric.LinearAlgebra as LA
import Data.Csv hiding ((.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import System.IO
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.IORef
import Control.Monad (void)
import Graphics.Rendering.Chart.Easy hiding (Vector, Matrix, set)
import qualified Graphics.Rendering.Chart.Easy as Chart
import Graphics.Rendering.Chart.Backend.Cairo
import System.Directory (getTemporaryDirectory)
import Control.Monad (when)
import System.Process (system)

main :: IO ()
main = do
    _ <- initGUI
    window <- setupWindow
    vbox <- setupLayout window
    thetaRef <- newIORef Nothing 
    setupHandlers vbox thetaRef
    widgetShowAll window
    mainGUI

setupWindow :: IO Window
setupWindow = do
    window <- windowNew
    Graphics.UI.Gtk.set window [ windowTitle := T.pack "Linear Regression"
                               , windowDefaultWidth := 500
                               , windowDefaultHeight := 500]
    return window

setupLayout :: Window -> IO VBox
setupLayout window = do
    vbox <- vBoxNew False 10
    containerAdd window vbox
    return vbox

setupHandlers :: VBox -> IORef (Maybe (LA.Vector Double)) -> IO ()
setupHandlers vbox thetaRef = do
    fileLabel <- labelNew (Just (T.pack "Choose CSV file:"))
    fileChooser <- fileChooserButtonNew (T.pack "Select CSV") FileChooserActionOpen
    trainButton <- buttonNewWithLabel (T.pack "Train")
    inputLabel <- labelNew (Just (T.pack "Enter comma-separated values:"))
    inputEntry <- entryNew
    predictButton <- buttonNewWithLabel (T.pack "Predict")
    visualizeButton <- buttonNewWithLabel (T.pack "Visualize") 
    resultLabel <- labelNew (Just (T.pack "Predicted Output: "))

    boxPackStart vbox fileLabel PackNatural 5
    boxPackStart vbox fileChooser PackNatural 5
    boxPackStart vbox trainButton PackNatural 5
    boxPackStart vbox inputLabel PackNatural 5
    boxPackStart vbox inputEntry PackNatural 5
    boxPackStart vbox predictButton PackNatural 5
    boxPackStart vbox visualizeButton PackNatural 5 
    boxPackStart vbox resultLabel PackNatural 5

    trainButton `on` buttonActivated $ do
        filePath <- fileChooserGetFilename fileChooser
        case filePath of
            Just file -> do
                (x, y) <- readCSV file
                let theta = normalEquation x y
                writeIORef thetaRef (Just theta)
                labelSetText resultLabel (T.pack "Model trained successfully!")
            Nothing -> labelSetText resultLabel (T.pack "Please select a CSV file.")

    predictButton `on` buttonActivated $ do
        thetaMaybe <- readIORef thetaRef
        case thetaMaybe of
            Just theta -> do
                inputText <- entryGetText inputEntry
                let inputValues = map read (wordsWhen (==',') inputText) :: [Double]
                let prediction = predict theta (LA.vector inputValues)
                labelSetText resultLabel (T.pack ("Predicted Output: " ++ show prediction))
            Nothing -> labelSetText resultLabel (T.pack "Please train the model first.")

    visualizeButton `on` buttonActivated $ do
        thetaMaybe <- readIORef thetaRef
        case thetaMaybe of
            Just theta -> do
                filePath <- fileChooserGetFilename fileChooser
                case filePath of
                    Just file -> do
                        (x, y) <- readCSV file
                        let predictions = map (predict theta) (LA.toRows x)
                        inputText <- entryGetText inputEntry
                        let currentPrediction = case wordsWhen (==',') inputText of
                                [] -> Nothing
                                inputValues -> 
                                    let inputVector = LA.vector (map read inputValues)
                                        predictedValue = predict theta inputVector
                                    in Just (last (LA.toList y), predictedValue)
                        plotGraph (LA.toList y) predictions theta currentPrediction
                        labelSetText resultLabel (T.pack "Graph visualized successfully!")
                    Nothing -> labelSetText resultLabel (T.pack "Please select a CSV file.")
            Nothing -> labelSetText resultLabel (T.pack "Please train the model first.")

    window <- widgetGetToplevel vbox
    void $ window `on` deleteEvent $ liftIO mainQuit >> return False

readCSV :: FilePath -> IO (LA.Matrix Double, LA.Vector Double)
readCSV file = do
    csvData <- BL.readFile file
    case decode NoHeader csvData of
        Left err -> error err
        Right v -> do
            let rows = V.toList v
                x = LA.fromLists (map init rows)
                y = LA.fromList (map last rows)
            return (x, y)

normalEquation :: LA.Matrix Double -> LA.Vector Double -> LA.Vector Double
normalEquation x y = (LA.inv (LA.tr x LA.<> x) LA.<> LA.tr x) LA.#> y

predict :: LA.Vector Double -> LA.Vector Double -> Double
predict theta x = theta LA.<.> x

plotGraph :: [Double] -> [Double] -> LA.Vector Double -> Maybe (Double, Double) -> IO ()
plotGraph actual predicted theta currentPrediction = do
    tmpDir <- getTemporaryDirectory
    let filePath = tmpDir ++ "/training_graph.png"
    toFile Chart.def filePath $ do
        layout_title Chart..= "Training Data vs Predictions"
        layout_x_axis . laxis_title Chart..= "Actual"
        layout_y_axis . laxis_title Chart..= "Predicted"
        plot (points "Actual vs Predicted" (zip actual predicted))
        case currentPrediction of
            Just (actualValue, predictedValue) -> do
                let currentPointStyle = def
                        & point_color .~ opaque red
                        & point_shape .~ Chart.PointShapeCircle
                        & point_radius .~ 5
                plot (points "Current Prediction" [(actualValue, predictedValue)])
            Nothing -> return ()

    putStrLn $ "Graph saved to: " ++ filePath
    _ <- system $ "xdg-open " ++ filePath
    return ()

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'

-- ghc -o main main.hs -package gtk3 -package hmatrix -package cassava -package vector -package Chart -package Chart-cairo