local simple_function(x, y) = x + y;

local multiline_function(x) =
  local temp = x * 2;
  [temp, temp + 1];

multiline_function(
  simple_function(1, 2)
)
