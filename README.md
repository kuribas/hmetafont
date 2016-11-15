# hmetafont

Hmetafont is a DSL (Domain Specific Language) for creating variable
fonts in haskell.  It's inspired by metafont, but adapted to modern
technologies (Opentype).  It's written as a library for haskell, with
a syntax that closely resembles metafont.  Some features are:

   * generates standard compliant unicode opentype fonts
   * simple and composite glyphs
   * specifying kerning pairs
   * specifying points in terms of parameters and equations
   * Creating paths using hobby splines or cubic beziers
   * filling and erasing paths
   * Calligraphic strokes with elliptic pens or pens from arbitrary
     (convex) paths.
   
It's work in progress.  Most of the work right now has gone into the
support libraries, which can be found here:

   * [cubicbezier](https://github.com/kuribas/cubicbezier): bezier
   intersections, boolean path operations, hobby splines, curve
   approximation, caligraphic strokes
   * [MFSolve](https://github.com/kuribas/mfsolve): A linear equation
     solver, with the ability to calculate non-linear expressions on
     the fly.
   * [Opentype](https://github.com/kuribas/haskell-opentype): Reading
     and writing of Opentype files into haskell

These packages can also be found on hackage:

   * [Opentype](http://hackage.haskell.org/package/opentype)
   * [MFSolve](http://hackage.haskell.org/package/mfsolve)
   * [Cubicbezier](http://hackage.haskell.org/package/cubicbezier)

## Example Program:

For example take the lowercase letter c from a metafont [(walbaum)](https://github.com/kuribas/walbaum-metafont):

```
beginchar("c", 12u#, x_height#, dp#);
  pickup tiny.nib;
  pos1(vair, 158);
  pos3(curve, 0);
  pos4(vair, -90);
  pos5(flare, flareangle);
  
  top y1r = 0.35x_height;
  rt x1l = x5r + 0.04w;
  
  x2r = 0.5[rt x3r, lft x1r];
  bot y2l = -o;
  top y2r = bot y2l + 0.05 [hair, stem];

  lft x3l = 0;
  y3 = x_height/2;

  x4 = x2l = w/2;
  top y4l = x_height + o;

  z2mid = 0.78[(x2r,y3r), (x3r,y2r)];
  x1mid = x2r + (x2r - x2mid);
  y1mid = y2mid; 

  lft x5l = 0.2[w - curve, x4r];
  bot y6 = 0.66h;
  z6 = z5 + whatever*dir (flareangle - 85);
  y5 = 2/3[y4, y6];

  penlabels(1,2,3,4,5,6);

  filldraw z1l{dir -100}..tension 0.75 and 0.78..z2l{left}..tension 0.88..z3l{up}..tension 0.88..
  z4l{right}..tension 1 and 1..z5r{dir (flareangle - 90)}..tension 0.85.. z6{dir(flareangle - 180)}.. tension 0.8..z5l{dir (flareangle + 90)}..tension 0.85..
  z4r{left}..tension 0.88..z3r{down}..z2mid{z2r-z3r}..z2r{right}..tension 0.9 and 0.75 ..{dir 80}z1r--cycle;
endchar;
```

In hmetafont this becomes:

```haskell
lowerC :: Glyph ()
lowerC = glyph "c" (12*u) &
  contours $=
  do pickup tinyNib
     pos 1 vair 158
     pos 2 curve 0
     pos 4 vair (-90)
     pos 5 flare flareangle

     top y1r === 0.35*x_height
     rt x1l === x5r + 0.04*w

     x2r === between 0.5 (rt x3r) (lft x1r)
     bot y2l === -o
     top y2r === bot y2l + between 0.5 hair stem

     left x3l === 0
     y3 === x_height/2

     x4 === x2l
     x2l === x/2
     top y4l === x_height + o

     z"2mid" === between 0.78 (x2r, y3r) (x3r, y2r)
     x"1mid" === x2r + (x2r - x"2mid")
     y"1mid" == y"2mid"

     lft x5l === between 0.2 (w - curve) x4r
     bot y6 === 0.66*h
	 w <- whatever
     z6 === z5 + w*dir (flareangle - 85)
     y5 === between (2/3) y4 y6

	 penlabels [1, 2, 3, 4, 5, 6]

     filldraw $ z1l .- leaving (dir (-100)) <> tensions 0.75 0.78 -.
       z2l .- leaving left <> tension 0.88 -.
	   z3l .- leaving up <> tension 0.88 -.
	   z4l .- leaving right -.
	   z5r .- leaving (dir (flareangle - 90)) <> tension 0.85 -.
	   z6 .- leaving (dir(flareangle - 180)) <> tension 0.8 -.
       z5l .- leaving (dir (flareangle + 90)) <> tension 0.85 -.
	   z4r .- leaving left <> tension 0.88 -.
	   z3r -. leaving down -.
	   z"2mid" .- leaving (z2r-z3r) -.
	   z2r .- leaving right <> tensions 0.9 0.75 <> arriving (dir 80) -.
	   z1r .--. cyclePath
```
