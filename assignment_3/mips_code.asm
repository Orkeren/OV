  LABEL l_while
    addi $1, $1, w
    bne  $1, $zero, l_true0
    j    l_false
  LABEL l_true0
    addi $2, $2, w
    addi $3, $3, v
    div $2, $3
    bne $LO, $zero, l_true1
    j   l_false
  LABEL l_true1
    addi $4, $4, w
    addi $5, $5, v
    blt $4, $5, l_true2
    j   l_false2
  LABEL l_true2
    addi $6, $6, v
    addi $7, $7, w
    sub v, $6, $7
    j l_while
  LABEL l_false2
    addi $8, $8, w
    addi $9, $9, v
    sub w, $8, $9
    j l_while
  LABEL l_false