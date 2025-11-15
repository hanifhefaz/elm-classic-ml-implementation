module Test.Generated.Main exposing (main)

import Example
import KMeansTest
import KNNTest
import RegressionTest
import VectorTest

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport UseColor
        , seed = 48577902255509
        , processes = 12
        , globs =
            []
        , paths =
            [ "C:\\Users\\Administrator\\Desktop\\elm-ml\\tests\\Example.elm"
            , "C:\\Users\\Administrator\\Desktop\\elm-ml\\tests\\KMeansTest.elm"
            , "C:\\Users\\Administrator\\Desktop\\elm-ml\\tests\\KNNTest.elm"
            , "C:\\Users\\Administrator\\Desktop\\elm-ml\\tests\\RegressionTest.elm"
            , "C:\\Users\\Administrator\\Desktop\\elm-ml\\tests\\VectorTest.elm"
            ]
        }
        [ ( "Example"
          , [ Test.Runner.Node.check Example.suite
            ]
          )
        , ( "KMeansTest"
          , [ Test.Runner.Node.check KMeansTest.tests
            ]
          )
        , ( "KNNTest"
          , [ Test.Runner.Node.check KNNTest.tests
            ]
          )
        , ( "RegressionTest"
          , [ Test.Runner.Node.check RegressionTest.tests
            ]
          )
        , ( "VectorTest"
          , [ Test.Runner.Node.check VectorTest.tests
            ]
          )
        ]