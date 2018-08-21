-- ex03.hs

module ChapterExercises where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woop"

frappe :: String -> String
frappe = flippy "haha"

-- 1) appedCatty "woohoo!" = "woop mrow woohoo!"
-- 2) frappe "1" = "1 mrow haha"
-- 3) frappe (appedCatty "2") = "woops mrow 2 mrow haha"
-- 4) appedCatty (frappe "blue") = "woops mrow blue mrow haha"
-- 5) cattyConny (frappe "pink")
--               (cattyConny "green" (appedCatty "blue") =
--    cattyConny (frappe "pink")
--               (cattyConny "green" "woop mrow blue") =
--    cattyConny (frappe "pink)
--               "green mrow woop mrow blue" =
--    cattyConny "pink mrow haha" "green mrow woop mrow blue" =
--      "pink mrow haha mrow green mrow woop mrow blue"
-- 6) cattyConny (flippy "Pugs" "are") "awesome" =
--    cattyConny "are mrow Pugs" "awesome" =
--    "are mrow Pugs mrow awesome"
