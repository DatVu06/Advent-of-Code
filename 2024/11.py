### Part 1
input = open("input.txt", "r")
lst = list(map(int, input.read().strip().split()))

dp = {}

def calc(num, step):
    if (num, step) in dp:
        return dp[(num, step)]
    if step == 0:
        return 1
    if num == 0:
        dp[(num, step)] = calc(1, step - 1)
        return dp[(num, step)]
    
    num_str = str(num)
    if len(num_str) % 2 == 0:
        mid = len(num_str) // 2
        dp[(num, step)] = calc(int(num_str[:mid]), step - 1) + calc(int(num_str[mid:]), step - 1)
        return dp[(num, step)]
    
    dp[(num,step)] = calc(num * 2024, step - 1)
    return dp[(num, step)]

ans1 = 0
for x in lst:
    ans1 += calc(x, 25)

print(ans1)

### Part 2
ans2 = 0
for x in lst:
    ans2 += calc(x, 75)

print(ans2)