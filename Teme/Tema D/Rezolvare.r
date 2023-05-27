# D1
# a)
x = c(1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3)
M = 1000000
find_M_element = function(x, M)
{
    n = length(x)
    for (i in 1:M)
    {
        index = sample(1:n, 1)
        if (sum(x == x[index]) >= n / 2 + 1)
            return(x[index])
    }
    return("x nu are M-element")
}
result = find_M_element(x, M)
print(result)

# b)
k = ceiling(log2(1/(10^7)))
print(k)

#================================================================================

# D2
A = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
i = 5
elementith = function(i, A)
{
    n=length(A)
    if (n == 1)
        return(A)

    z = sample(A, 1)
    A_mic = A[A < z]
    A_mare = A[A > z]

    if (length(A_mic) >= i)
        return(elementith(i, A_mic))
    else
        if (n - length(A_mare) >= i)
            return(z)
        else
            return(elementith(i - n + length(A_mare), A_mare))
}
elementith(i, A)

#================================================================================

# D3
# a)
SORT = function(v)
{
    n = length(v)
    for (i in 1:(n - 1))
    {
        min_index = i
        for (j in (i + 1):n)
            if (v[j] < v[min_index])
                min_index = j

        temp = v[i]
        v[i] = v[min_index]
        v[min_index] = temp
    }
    return(v)
}

S = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
a = 0.5
mediana = function(S, a)
{
    n = length(S)
    m = floor(a * log(n))

    s = sample(S, m)
    s = SORT(s)

    # Returnam mediana lui S'
    if (m %% 2 == 0)
        valoarea_med = (s[m / 2] + s[m / 2 + 1]) / 2
    else
        valoarea_med = s[(m + 1) / 2]

    return(valoarea_med)
}
mediana(S, a)

#VARIANTA
#a)
mediana = function(S, a)
{
  n = length(S)
  m = floor(a * log(n))
  S1 = sample(S, m)
  sorted_S1 = sort(S1)

  median_value = sorted_S1[floor(m/2) + 1]
  return(median_value)
}

S = c(1, 5, 3, 9, 2, 7, 4, 6, 8, 10)
a = 0.5
mediana(S, a)

# b)
dim_minimum = function(a, prob)
{
    n = 1
    while (TRUE)
    {
        S = sample(1:n, n, TRUE)

        p = 1 - 2 / n^2
        if (p >= prob)
            break
        n = n + 1
    }
    return(n)
}
dim_minimum(0.5, 1 - 1e-7)