import Escape (escape)
import Parse (parse)
import Text.Printf (printf)

main :: IO ()
main =
  interact $
    unlines
      . map (uncurry (printf "\n%s%s") . (show <$>))
      . escape
      . parse
