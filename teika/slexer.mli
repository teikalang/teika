val from_string :
  filename:string ->
  (Sparser.token, 'a) MenhirLib.Convert.traditional ->
  string ->
  'a
