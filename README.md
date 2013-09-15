# Visual Debugger
## A16Z Hackathon Project

This is a stepping debugger that draws a visualization of all the data structures in scope. It works on a small, imperative subset of Python---basically pseudocode like you'd see in an algorithms textbook.

Currently, it can display scalars, lists and graphs, which is enough for a bunch of interesting graph algorithms. I think this could be a good tool for teaching these algorithms to students: they are much easier to think about when you can see what's going on in front of you instead of trying to keep everything in your head.

The tool is written in Haskell, using the new and shiny threepenny-gui library for the web frontend. The library was easy to use but unfortunately somewhat buggy. Admittedly, I don't know how much of that was my fault and how much the library's!

This was certainly a fun project.