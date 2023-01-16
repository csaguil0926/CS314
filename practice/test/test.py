def transpose(m):
    height = len(m)
    width = len(m[0])
    return  [[m[i][j] for i in range(height)] for j in range(width)]


A = [[1,2,3],[4,5,6]]
trans_A = transpose(A)
print(trans_A)