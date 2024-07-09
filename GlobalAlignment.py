import copy

# This is just a basic program coded in Python to compare two sequences and align them pairwise using
# simplified version of Needleman-Wunsch algorithm the global alignment method.
# - The input sequences are scored pairwise and the scores and save in a 2D list named alignMatrix,
# - and the memory of the movements within the matrix is saved in another matrix called vectorMatrix
# with number corresponding to all possible 6 movements and combinations thereof,
# - and at last aligned on top of each other accordingly.
# Pls note: This version does not present all the possible movements or alignments
# when, for example, two different movements are possible.
# For instance, a match will be executed when at least another decision is possible
# such as mismatch or a gap as long as the scores would be the same.
# Or a gap in the first sequence is preferred instead of that in the second
# (a vertical movement if the first sequence is written on top of the matrix).


seq1 = input("Enter the first sequence: ")
seq2 = input("Enter the second sequence: ")
alignMatrix = []
rows = [0]
for letter in seq1:
    rows.append(0)
alignMatrix.append(rows.copy())
for letter in seq2:
    alignMatrix.append(rows.copy())
vectorMatrix = copy.deepcopy(alignMatrix)


def entscheidung(x, y):
    match = alignMatrix[x][y] + 5
    mismatch = alignMatrix[x][y] - 4
    gap_down = alignMatrix[x][y + 1] - 2
    gap_right = alignMatrix[x + 1][y] - 2
    if seq1[y] == seq2[x]:
        if match > gap_down and match > gap_right:
            alignMatrix[x + 1][y + 1] = match
            vectorMatrix[x + 1][y + 1] = 0
        elif match == gap_right and match == gap_down:
            alignMatrix[x + 1][y + 1] = match
            vectorMatrix[x + 1][y + 1] = 6
        elif match == gap_down and match > gap_right:
            alignMatrix[x + 1][y + 1] = match
            vectorMatrix[x + 1][y + 1] = 3
        elif match == gap_right and match > gap_down:
            alignMatrix[x + 1][y + 1] = match
            vectorMatrix[x + 1][y + 1] = 4
        elif gap_down > gap_right:
            alignMatrix[x + 1][y + 1] = gap_down
            vectorMatrix[x + 1][y + 1] = 1
        elif gap_right > gap_down:
            alignMatrix[x + 1][y + 1] = gap_right
            vectorMatrix[x + 1][y + 1] = 2
        else:
            alignMatrix[x + 1][y + 1] = gap_right
            vectorMatrix[x + 1][y + 1] = 5
    elif mismatch > gap_down and mismatch > gap_right:
        alignMatrix[x + 1][y + 1] = mismatch
        vectorMatrix[x + 1][y + 1] = 0
    elif mismatch == gap_down and mismatch == gap_right:
        alignMatrix[x + 1][y + 1] = mismatch
        vectorMatrix[x + 1][y + 1] = 6
    elif mismatch == gap_down and mismatch > gap_right:
        alignMatrix[x + 1][y + 1] = mismatch
        vectorMatrix[x + 1][y + 1] = 3
    elif mismatch == gap_right and mismatch > gap_down:
        alignMatrix[x + 1][y + 1] = mismatch
        vectorMatrix[x + 1][y + 1] = 4
    elif gap_down > gap_right:
        alignMatrix[x + 1][y + 1] = gap_down
        vectorMatrix[x + 1][y + 1] = 1
    elif gap_right > gap_down:
        alignMatrix[x + 1][y + 1] = gap_right
        vectorMatrix[x + 1][y + 1] = 2
    else:
        alignMatrix[x + 1][y + 1] = gap_right
        vectorMatrix[x + 1][y + 1] = 5


col = 0
for i in seq1:
    row = 0
    alignMatrix[0][col + 1] = alignMatrix[0][col] - 2
    vectorMatrix[0][col + 1] = 1
    for j in seq2:
        alignMatrix[row + 1][0] = alignMatrix[row][0] - 2
        vectorMatrix[row + 1][0] = 1
        entscheidung(row, col)
        row += 1
    col += 1

sqal1 = ""
sqal2 = ""

m = 1
n = 1
print("Girilen sekans uzunlukları sırasıyla: ", len(seq1), len(seq2))
while m <= len(seq1) and n <= len(seq2):
    if vectorMatrix[-n][-m] == 0 or vectorMatrix[-n][-m] == 3 or vectorMatrix[-n][-m] == 4 or vectorMatrix[-n][-m] == 6:
        sqal1 = seq1[-m] + sqal1
        sqal2 = seq2[-n] + sqal2
        m += 1
        n += 1
    elif vectorMatrix[-n][-m] == 1 or vectorMatrix[-n][-m] == 5:
        sqal1 = '_' + sqal1
        sqal2 = seq2[-n] + sqal2
        n += 1
    elif vectorMatrix[-n][-m] == 2:
        sqal1 = seq1[-m] + sqal1
        sqal2 = '_' + sqal2
        m += 1

print(sqal1)
print(sqal2)
