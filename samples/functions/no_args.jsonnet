local greet() = 'hello';
local make_obj() = { x: 1, y: 2 };
local apply(f) = f();
local obj = { m(): 7 };

{
  greet: greet(),
  obj: make_obj(),
  inline: (function() 42)(),
  applied: apply(function() 'world'),
  method_call: obj.m(),
}
