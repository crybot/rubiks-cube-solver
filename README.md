# rubiks-cube-solver
## A Learning based Rubik's cube solver

At the moment this project is nothing more than a partially working Rubik's cube visualizer.
The code is modular in the sense that the Cube model and its state transformations are completely independent from the
graphics and can be used to implement any sort of solving algorithm.
The Cube type itself is pretty-printable.

In the later stages of development, the project should be able to solve a scrambled Rubik's cube without human intervention
during the learning phase.

### TODOs:
- Implement relative rotations (at the moment they are partially global)
- Solve aliasing problem: does LambdaCube 3D allow antialiasing?
- Add a solver

### Preview
![Alt Text](https://media.giphy.com/media/E0RTTTB1xuYYalkEec/giphy.gif)

