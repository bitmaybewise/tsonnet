// binding closure
local id = function(x) x;
// immediately called closure
(function(x) id(x) * id(x))(5)
