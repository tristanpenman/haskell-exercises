# Notes for HW1

## Towers of Hanoi

It is not too difficult to solve this in Haskell using a recursive algorithm. However, even after implementing an algorithm that I found online, I still found it difficult to visualise how it works.

Here are the ASCII art drawings that I used to better understand the algorithm.

### Two disks

ASCII art representation of recursive solution for two disks. For reference, here is the function:

    hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

    -- base case
    hanoi 0 _ _ _ = []

    -- redundant, but included for illustration
    hanoi 1 a b c = [(a, b)]

    -- recursive case
    hanoi n a b c = left ++ [(a, b)] ++ right
        where left  = hanoi (n-1) a c b
              right = hanoi (n-1) c b a

We want to move two disks on peg A to peg B, using C as a temporary disk.

     |    |    |
     _    |    |
    ___   |    |

This is how we'd call this function:

    hanoi 2 a b c

This will first move the top-most disk on A to C, the temporary peg, to 'release' the next disk.

    hanoi (n - 1) a c b  ==  hanoi 1 a c b  ==  [(a, c)]

     |    |    |
     |    |    |
    ___   |    _

Now we can move the next disk from A to B:

    [(a, b)]

    |    |    |
    |    |    |
    |   ___   _

Now we can move the disk from C to A, undoing what we did before:

    hanoi (n - 1) c b a  ==  hanoi 1 c b a  ==  [(c, b)]


    |    |    |
    |    _    |
    |   ___   |

### Three disks

Three disks is a bit more complex:

      _       |       |
     ___      |       |
    _____     |       |

This is how we'd call the function:

    hanoi 3 a c b

First a 2-disk sub-problem will be solved, moving the first two disks from A to C, using B as a temporary peg:

    hanoi (n - 1) a c b  ==  hanoi 2 a c b  ==  [(a, b), (a, c), (b, c)]

      |       |       |
      |       |       _
    _____     |      ___

Now the last disk from A will be moved to B:

    [(a, b)]

      |       |       |
      |       |       _
      |     _____    ___

Finally, another 2 disk sub-problem will be solved, moving the last two disks from C to B, using A as a temporary peg:

    hanoi (n - 1) c b a  ==  hanoi 2 c b a  ==  [(c, a), (c, b), (a, b)]

      |       _       |
      |      ___      |
      |     _____     |
