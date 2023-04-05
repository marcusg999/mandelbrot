# Mandelbrot
Mandelbrot Haskell Project:
This program generates a Mandelbrot set image as a text-based ASCII art. It takes four arguments: width, height, maxIterations, and steps. The width and height determine the size of the image, the maxIterations determine how long to iterate the function for each pixel, and the steps determine the spacing between each pixel. If the program is run with no arguments, it will default to a 80x40 image with 100 iterations and a step size of 1.
The mandelbrot function generates the Mandelbrot set as a 2D list of integers, where each integer represents the number of iterations it took for the corresponding pixel to escape to infinity. The printMandelbrot function converts this 2D list of integers into a string of ASCII art using ANSI escape codes to color each pixel based on the number of iterations.

After setting up project enter runhaskell main.hs width, height, maxIterations, and steps in the app folder in your terminal. 
I.E  Example runhaskell main.hs 160 80 200 2
