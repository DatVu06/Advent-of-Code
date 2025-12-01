### Part 1
input = open("input.txt", "r")
str = input.read().strip()
gen = []

empty = False
id = 0
for num in str:
    if not empty:
        empty = True
        gen.extend([id] * int(num))
        id += 1
    else:
        empty = False
        gen.extend([-1] * int(num))

sz = len(gen)
l, r = 0, sz - 1
ans1 = 0
while l <= r:
    add = gen[l]
    if gen[l] == -1:
        while r > l:
            if gen[r] != -1:
                add = gen[r]
                r -= 1
                break
            r -= 1
        if l == r:
            break
    
    ans1 += l * add
    l += 1

print(ans1)


### Part 2
def findPos(lim, length, rep):
    i = 0
    while i + length - 1 < lim:
        if gen[i] != -1:
            i += 1
            continue
        cnt = 1
        while i + cnt < lim and gen[i + cnt] == -1:
            cnt += 1
        if cnt < length:
            i += cnt
            continue
        
        for j in range(length):
            gen[i + j] = rep
        return True
        
    return False

r = sz - 1
ans2 = 0
while r >= 0:
    if gen[r] == -1:
        r -= 1
        continue

    cnt = 1
    while r - cnt >= 0 and gen[r - cnt] == gen[r]:
        cnt += 1

    if findPos(r - cnt + 1, cnt, gen[r]):
        for i in range(cnt):
            gen[r - i] = -1
    
    r -= cnt

print(sz, id)

for i in range(sz):
    if gen[i] == -1:
        continue
    ans2 += gen[i] * i

print(ans2)
    