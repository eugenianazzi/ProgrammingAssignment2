## Estas funciones verifican si ya hay un valor calculado en cache antes de recalcularlo

## Función que crea una matriz especial, que tiene 4 propiedades.
## 1- setea la matriz a base de la que se le pasa
## 2- Una vez creada; podes ver cual era la matriz en sí
## 3- Se le puede setear la matriz inversa
## 4- Obtener la matriz inversa

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set_mat <- function(y_mat) {
        x <<- y_mat
        inv <<- NULL
    }
    get_mat <- function() x
    setinv <- function(invert) inv <<- invert
    getinv <- function() inv
    list(set = set_mat, get = get_mat,
         setinv= setinv,
         getinv = getinv)
}


## Esta función verifica si ya se ha calculado la matriz inversa
## Si ya se calculo, no la recalcula. Si no, la calcula y almacena.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'   
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv) # si ya lo tiene, lo muestra y termina la función
    }
    info_mat <- x$get()
    inv <- solve(info_mat) # calcula la inversa
    x$setinv(inv) # almacena el valor calculado
    inv
}
