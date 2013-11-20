3dlsystem
=========

Entry for the 2013 Informatics 1 Functional programming competition.

Several tutorials have aided in the making of this program:

* http://www.haskell.org/haskellwiki/OpenGLTutorial1
* http://twoguysarguing.wordpress.com/2010/02/20/opengl-and-haskell/
* http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.126.1682&rep=rep1&type=pdf

Must be compiled using ghc with the flag `-XExistentialQuantification`; This
enables a non-standard extential which allows the creating of arbitrary
heterogenius types. See the link below for details.
http://www.haskell.org/haskellwiki/Heterogenous_collections#Existential_types

I am not working with OpenGL's vector type for as much of this program as I
can, as my grasp of monads is shaky at best, and that of OpenGL even worse.

For program usage: To install run

`make` or, alternatively
`ghc --make -XExistentialQuantification 3DLSystem`

For a help message, run `./3DLSystem` without arguments.
