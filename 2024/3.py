import re

### Part 1
input = open("input.txt", "r")
str = input.read()

pattern = r"mul\((\d{1,3}),(\d{1,3})\)"
match = re.findall(pattern, str)
sum = 0
for a, b in match:
    sum += int(a) * int(b)

print(sum)


### Part 2
tokens = re.split(r"(mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\))", str)
sum2 = 0
flag = True
for token in tokens:
    if re.search(r"do\(\)", token):
        flag = True
    elif re.search(r"don't\(\)", token):
        flag = False
    elif re.search(pattern, token) and flag:
        match = re.findall(pattern, token)
        for a, b in match:
            sum2 += int(a) * int(b)

print(sum2)