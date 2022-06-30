import Competition.Game.Game (runGame)
import Competition.Tournament.Tournament (runTournament)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run [] = runTournament
run commits@[_, _] = runGame commits
