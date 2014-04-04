--Program:	find_links.hs
--Author:	Patrick Villanueva
--Class:	Programming Languages
--Purpose:	Extract links from an html webpage
--Usage:	ghc --make find_links
--			./find_links (-bycount) <filename>


module Main where

import Data.Function
import System.IO
import System.Environment   
import Data.List 
import Text.Regex.Posix
import qualified Data.Set as Set

main = do
  args <- getArgs
  let sortbycount = parse_bycount (args!!0)
  input <- parse_file sortbycount args
  let x = split (remove_spaces (parse_tag input))
  let list1 = apply_regex1 x
  let list2 = apply_regex2 list1
  let http_list = get_links list2
  let group_list = group (sort (http_list))
  let tuple_list = createTuple group_list
  let sorted_tuples = sort_this sortbycount tuple_list
  let to_print = format_string sorted_tuples
  
  writeFile "links.txt" to_print

bycount (a1, b1) (a2, b2)
  		| b1 < b2 = GT
  		| b1 > b2 = LT
  		| b1 == b2 = compare a1 a2

parse_bycount :: String -> Bool
parse_bycount x = if x == "-bycount" then True
									 else False

parse_file :: Bool -> [String] -> IO String
parse_file x xs = if x == True then fmap concat $ mapM readFile (tail xs)
					else fmap concat $ mapM readFile xs

sort_this :: Bool -> [(String, Int)] -> [(String, Int)]
sort_this bool x = if bool == True then sortBy bycount x
				   else sort x

format_string :: [(String, Int)] -> String
format_string x = if x == [] then ""
				  else fst (head x) ++ "," ++ show (snd (head x)) ++ "\n" ++ format_string (tail x)

print_list :: [String] -> IO ()
print_list x = if x /= [] then do 
								putStrLn(head x) 
								print_list (tail x) 
				else putStrLn("")

print_group :: [[String]] -> IO ()
print_group x = if x /= [] then do 
								putStrLn(head(head x)) 
								print_group (tail x) 
				else putStrLn("")

print_tuples :: [(String, Int)] -> IO ()
print_tuples x = if x /= [] then do 
								putStrLn(fst (head x) ++ "," ++ show (snd (head x))) 
								print_tuples (tail x) 
				else putStrLn("")

apply_regex1 :: [String] -> [String]
apply_regex1 x = if x /= [] && (head x =~ "<(a|A).*(href|HREF).*>" :: String) /= "" then [head x =~ "<(a|A).*(href|HREF).*>" :: String] ++ apply_regex1 (tail x)
				 else if x /= [] then apply_regex1 (concat_line x)
				 else []

apply_regex2 :: [String] -> [String]
apply_regex2 x = if x /= [] && (head x =~ "(href|HREF)=(\'|\")http.+" :: String) /= "" then [head x =~ "(href|HREF)=(\'|\")http.+" :: String] ++ apply_regex2 (tail x)
				 else if x /= [] then apply_regex2 (tail x)
				 else []

concat_line :: [String] -> [String]
concat_line x =  if (x /= []) && (tail x /= []) && (tail (tail x) /= []) then [head x ++ head (tail x)] ++ tail (tail x)
				 else if (x /= []) && (tail x /= []) then [head x ++ head (tail x)]
				 else []

get_links :: [String] -> [String]
get_links x = if x == [] then []
	             else if get_link (head x) /= "" then [get_link (head x)] ++ get_links (tail x)
	             else get_links (tail x)

get_link :: String -> String
get_link x = strip_link1 x

strip_link1 :: String -> String
strip_link1 x = if (head x == '\'') || (head x == '\"') then strip_link2 (tail x)
			  else strip_link1 (tail x)

strip_link2 :: String -> String
strip_link2 x = if (head x == '\'') || (head x == '\"') then ""
	         else [head x] ++ strip_link2 (tail x)

get_tags :: String -> String
get_tags x = parse_tag x

parse_tag :: String -> String
parse_tag x = if x == "" then ""
			  else if head x == '<' then "<" ++ parse_tag2 (tail x) ++ parse_tag (tail x)
			  else parse_tag (tail x)

parse_tag2 :: String -> String
parse_tag2 x = if head x /= '>' then [head x] ++ parse_tag2 (tail x)
			   else ">"

remove_spaces :: String -> String
remove_spaces x = if x == [] then ""
					else if head x == '\n' || head x == ' ' || head x == '\t' then remove_spaces (tail x)
					else [head x] ++ remove_spaces (tail x)

split :: String -> [String]
split x = lines (add_newlines x)

add_newlines :: String -> String
add_newlines x = if x == [] then ""
	         else if head x == '>' then ">\n" ++ add_newlines (tail x)
	         else [head x] ++ add_newlines (tail x)

createTuple :: [[String]] -> [(String, Int)]
createTuple x = if x == [] then [] 
				else [("\"" ++ ((head x)!!0 ++ "\""), length (head x))] ++ createTuple (tail x)
