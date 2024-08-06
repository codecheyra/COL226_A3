type token =
  | Identifier of string  (* Represents an identifier token containing a string value *)
  | Keyword of string   (* Represents a keyword token containing a string value. and so on........*)
  | Boolean of bool
  | Integer of int
  | Arithmeticoperator of string
  | Comparisonoperator of string
  | Stringoperator of string
  | Stringliteral of string
  | Parenthesis of char
  | Comma (* represents comma token. *)

let is_loweralphabet c = c >= 'a' && c <= 'z' (*check if the character is a lowercase alphabet*)
let is_upperalphabet c = c >= 'A' && c <= 'Z' (*check if the character is an uppercase alphabet*)
let is_digit c = c >= '0' && c <= '9' (*check if the character is a digit*)
let is_prime c = c = '\'' (*check if the character is a prime*)
let is_underscore c = c = '_' (*check if the character is an underscore*)

let rec take_while pred s =
  match s with
  | [] -> ([], [])  (* Base case: If the input list is empty, return an empty tuple. ([], []) taken x, remaining xs*)
  | x :: xs when pred x ->  (*If the predicate is true, take the element and continue*)
      let taken, remaining = take_while pred xs in  (*recursive call to take_while function*)
      (x :: taken, remaining) (*return the taken element and the remaining elements as a tuple of two lists*)
  | _ -> ([], s)  (*If the predicate is false, return an empty list and the remaining elements ([], [])*)
  (*example  = [] -> ([], [])*)
  (*example = if take_while (fun x -> x <= 3) [1; 2; 3; 4; 5] -> ([1; 2; 3], [4; 5]) *)

let rec tokenize_helper s tokens =  (* recursive function tokenize_helper takes two arguments: s=input string to be tokenized, 
   and tokens=accumulates the list of tokens generated during tokenization. *)
  match s with  (* pattern matching on input string s *)
  | [] -> List.rev tokens (* Base case: If the input string is empty, return the list of tokens in reverse order (stack datastructure)*)
  | c :: cs when is_loweralphabet c || is_underscore c -> (* checks if first character is lower alphabet or underscore *)
      let ident, remaining = take_while (fun x -> is_loweralphabet x || is_upperalphabet x || is_digit x || is_prime x || is_underscore x) s in
      (* take while is a function that extracts the sequence of characters forming an identifier in the input string s *)
      let token = String.concat "" (List.map (String.make 1) ident) in
      (* convert the sequence of characters into a string forming identifier *)

      if token = "if" || token = "then" || token = "else" then  (* checks if the token is one of the keywords: "if", "then", or "else". *)
        tokenize_helper remaining (Keyword token :: tokens) (* recursive call for remaining portion of the respective string *)
      else
        tokenize_helper remaining (Identifier token :: tokens)
  | c :: cs when is_digit c ->  (* pattern matching if first occurance is digit *)
      let digits, remaining = take_while is_digit s in  (* take_while finction extracts the longest prefix of digits from string
         returns both extracted digits and the remaining portion of the string.*)
      let number = int_of_string (String.concat "" (List.map (String.make 1) digits)) in
      (* The extracted digits are concatenated into a single string using String.concat and converted to an integer using int_of_string. *)
      tokenize_helper remaining (Integer number :: tokens)
      (* integer token is appended to list of tokens and recursive call to tokeniser_help to perform the same to remaining portion of the string. *)
  | '+' :: remaining -> tokenize_helper remaining (Arithmeticoperator "+" :: tokens)
  (* pattern matcheing to the case where the first character in the input string is '+', indicating an arithmetic operator token (Arithmeticoperator "+"). 
     and then recursive call tokenize_helper with the remaining characters remaining. 
     
     same for all the below with respective operations and its respective operator name. *)

  | '-' :: remaining -> tokenize_helper remaining (Arithmeticoperator "-" :: tokens)
  | '*' :: remaining -> tokenize_helper remaining (Arithmeticoperator "*" :: tokens)
  | '/' :: remaining -> tokenize_helper remaining (Arithmeticoperator "/" :: tokens)
  | '=' :: remaining -> tokenize_helper remaining (Comparisonoperator "=" :: tokens)
  | '<' :: '=' :: remaining -> tokenize_helper remaining (Comparisonoperator "<=" :: tokens)
  | '>' :: '=' :: remaining -> tokenize_helper remaining (Comparisonoperator ">=" :: tokens)
  | '<' :: remaining -> tokenize_helper remaining (Comparisonoperator "<" :: tokens)
  | '>' :: remaining -> tokenize_helper remaining (Comparisonoperator ">" :: tokens)
  | '&' :: '&' :: remaining -> tokenize_helper remaining (Boolean true :: tokens)
  | '|' :: '|' :: remaining -> tokenize_helper remaining (Boolean false :: tokens)
  | '"' :: remaining ->
      let str, remaining = take_while (fun x -> x <> '"') remaining in  (* uses take_while to extract characters until the next double quote. *)
      let string_literal = String.concat "" (List.map (String.make 1) str) in (* Concatenates the extracted characters into a string. *)
      tokenize_helper (List.tl remaining) (Stringliteral string_literal :: tokens)  (* Appends the string literal token to the list of tokens. *)
  | '(' :: remaining -> tokenize_helper remaining (Parenthesis '(' :: tokens)
  | ')' :: remaining -> tokenize_helper remaining (Parenthesis ')' :: tokens)
  | ',' :: remaining -> tokenize_helper remaining (Comma :: tokens)
  | ' ' :: remaining | '\n' :: remaining | '\t' :: remaining -> tokenize_helper remaining tokens
  | _ :: remaining -> tokenize_helper remaining tokens

let tokenize s = tokenize_helper (List.of_seq (String.to_seq s)) []
(* Converts the input string 's' into a sequence of characters using String.to_seq, then converts it into a list using List.of_seq.
   Calls the tokenize_helper function to tokenize the list of characters, initializing the list of tokens as an empty list.
   Returns the list of tokens generated from the input string. *)

(* List of test cases as strings each list represent a single test case in the form of a string. *)
let () =
  let test_cases = [
    "x = 10 + 20";
    "if x >= 5 then y = 10 else y = 20";
    "x' = 2 * x";
    "_abc";
    "y = x / 5";
    "z = (x > y) && (y < z)";
    "name = \"John\"";
    "concat = \"Hello, \" + name";
    "pair = (3, 4)";
    "result = if x <= y then \"Less\" else \"Greater\"";
    "ifthen 1abc Abc abc12 _abc";
    "\"hello abc\"";
  ] in
  List.iter (fun test_case -> (* iterates over each test case in testcases list. *)
    let tokens = tokenize test_case in  
    (* tokenizes the current test case using the 'tokenize' function and stores the tokens in 'tokens' list. *)
    Printf.printf "Test case: %s\n" test_case;
    (* description of curent test case. *)
    List.iter (fun token ->
      match token with  (* iterate over each token in the 'tokens' list using pattern matching algorithm. *)
      | Identifier s -> Printf.printf "Identifier: %s\n" s
      | Keyword s -> Printf.printf "Keyword: %s\n" s
      | Boolean b -> Printf.printf "Boolean: %b\n" b
      | Integer i -> Printf.printf "Integer: %d\n" i
      | Arithmeticoperator s -> Printf.printf "Arithmetic operator: %s\n" s
      | Comparisonoperator s -> Printf.printf "Comparison operator: %s\n" s
      | Stringoperator s -> Printf.printf "String operator: %s\n" s
      | Stringliteral s -> Printf.printf "String literal: %s\n" s
      | Parenthesis c -> Printf.printf "Parenthesis: %c\n" c
      | Comma -> Printf.printf "Comma\n"
    ) tokens;
    Printf.printf "\n"  (* print a new line. *)
  ) test_cases;;  (* execute all test cases iteratively each at once. and then terminate. *)