import re

### Part 1
input = open("input.txt", "r")

ans1 = 0
numberlist = []
for line in input:
    numbers = list(map(int, re.findall(r'\d+', line)))
    numberlist.append(numbers)
    dict = {}
    dict[numbers[1]] = True
    for i in range(2, len(numbers)):
        cur = {}
        for key in dict:
            if key + numbers[i] <= numbers[0]:
                cur[key + numbers[i]] = True
            if key * numbers[i] <= numbers[0]:
                cur[key * numbers[i]] = True
        dict = cur
    if numbers[0] in dict:
        ans1 += numbers[0]

print(ans1)


### Part 2
ans2 = 0
for numbers in numberlist:
    dict = {}
    dict[numbers[1]] = True
    for i in range(2, len(numbers)):
        cur = {}
        for key in dict:
            if key + numbers[i] <= numbers[0]:
                cur[key + numbers[i]] = True
            if key * numbers[i] <= numbers[0]:
                cur[key * numbers[i]] = True
            if len(str(key)) + len(str(numbers[i])) <= len(str(numbers[0])) and int(str(key) + str(numbers[i])) <= numbers[0]:
                cur[int(str(key) + str(numbers[i]))] = True
        dict = cur
    if numbers[0] in dict:
        ans2 += numbers[0]

print(ans2)