Token If      | Instruction
--------------|------------
if            | condition check
elif          | condition check not met jump to next
else          | jump to end & address
end           | address

Token While   | Instruction
--------------|------------
while         | address
do            | conditon not met jump to end
end           | jump back to while & address


Token Word    | Instruction
--------------|------------
word + id     | address
end           | return

Token Id Type | Instruction
--------------|------------
Const         | replace id with value
Word          | function call



Looking into Rust Ref
