from manim import *
from manim.animation.transform import Transform
from manim.mobject.mobject import Mobject
import random

class QuestionFiveChapter20H(Scene):
    def construct(self):
        number_plane = NumberPlane(background_line_style={
            "stroke_color": TEAL,
            "stroke_width": 4,
            "stroke_opacity": 0.4
        })
        number_plane.add_coordinates()

        ln = Line(number_plane.coords_to_point(100,100), number_plane.coords_to_point(-100,-100), color=RED)
        lnEqu = MathTex(r"y=m \cdot x + c", font_size=64).shift(UP + RIGHT*3.5)

        dt = Dot(number_plane.coords_to_point(2,3), color=YELLOW)
        df = dt.copy().set_color(GREEN_A)
        self.add(number_plane)
        self.play(FadeIn(ln,lnEqu))
        self.play(Wait(1))
        self.play(Transform(lnEqu, MathTex(r"y=m \cdot x + 0", font_size=64).shift(UP + RIGHT*3.5)))
        self.play(Wait(1))
        self.play(Transform(lnEqu, MathTex(r"y=m \cdot x", font_size=64).shift(UP + RIGHT*3)))
        
        tanRel = MathTex(r"m = \tan \theta", font_size=50, color = YELLOW_A).shift(UP*0.5 + RIGHT*3)
        
        self.play(Write(tanRel))
        self.play(Transform(lnEqu, MathTex(r"y=\tan \theta \cdot x", font_size=64).shift(UP + RIGHT*3)))
        self.play(Wait(1))
        self.play(Transform(lnEqu, MathTex(r"y=x \cdot \tan \theta", font_size=64).shift(UP + RIGHT*3)))
        self.play(Wait(1))

        self.play(Transform(tanRel, MathTex(r"1 = m = \tan \theta", font_size=50, color = YELLOW_A).shift(UP*0.5 +RIGHT*3)))
        self.play(Wait(1))
        self.play(Transform(tanRel, MathTex(r"1 = \tan \theta", font_size=50, color=YELLOW_A).shift(UP*0.35 + RIGHT*3)))
        self.play(Wait(1))
        self.play(Transform(tanRel, MathTex(r"\theta = \frac{\pi}{4}", font_size=50, color=YELLOW_A).shift(RIGHT*3)))
        self.play(Wait(1))



        mRotA = Matrix([[r"\cos \theta", r"- \sin \theta"],
                      [r"\sin \theta", r"\cos \theta"]]).scale(0.75)
        mProjectB = Matrix([[1,0],[0,0]]).scale(0.75)
        mRotC = Matrix([[r"\cos \theta", r"\sin \theta"],
                      [r"- \sin \theta", r"\cos \theta"]]).scale(0.75)
        
        # mRotProj = Matrix([[r"\cos \theta", 0],
        #                    [r"\sin \theta", 0]])
        
        mRotC.shift(UP*3+LEFT*4)
        # mRotProj.next_to(mRotC,LEFT)

        finalTrans = Matrix([[r"\cos^{2} \theta", r"\cos \theta \sin \theta"],
                              [r"\cos \theta \sin \theta", r"\sin^{2} \theta"]],h_buff=3)

        # self.play(Create(transGroup,lag_ratio=0))

        # self.play(FadeOut(mRotA, mProjectB))
        # self.play(Write(mRotProj,lag_ratio=0))

        # self.play(FadeOut(mRotProj, mRotC))
        # self.play(Write(finalTrans, lag_ratio=0))

        dtpos = Tex("(2,3)", font_size=45, color = YELLOW).next_to(dt, UP)
        dfpos = dtpos.copy().set_color(GREEN_A)
        self.play(Create(dt))
        self.play(FadeIn(dtpos))
        self.play(Wait(1))

        self.play(Write(mRotC))
        self.play(AnimationGroup(FadeIn(df,dfpos),
                                 Rotate(dt, angle = -PI/4, about_point=[0,0,0]), 
                                 Rotate(ln, angle = -PI/4),
                                 tanRel.animate.move_to([-2,-2,0]),
                                 mRotC.animate.shift([3,0,0]),
                                 lnEqu.animate.move_to([-2,-1,0]),
                                 Transform(dtpos, Tex("(x,y)", font_size=32, color = YELLOW).move_to(number_plane.coords_to_point(3.6,1.2)))
                                 ))
        self.play(Wait(2))
        
        self.play(Write(mProjectB.next_to(mRotC,LEFT)))
        self.play(AnimationGroup(
            dt.animate.move_to(dt.get_center()*[1,0,0]),
            Transform(dtpos, Tex("(x,0)", font_size=32, color = YELLOW).move_to(number_plane.coords_to_point(3.6,0.3)))
        ))
        self.play(Wait(2))
        
        self.play(Write(mRotA.next_to(mProjectB, LEFT)))
        self.play(AnimationGroup(Rotate(dt, angle = PI/4, about_point=[0,0,0]), 
                                 Rotate(ln, angle = PI/4),
                                 tanRel.animate.move_to([3.5,0,0]),
                                 lnEqu.animate.move_to([3.5,1,0]),
                                 Transform(dtpos, Tex("(2.5,2.5)", font_size=45, color = YELLOW).move_to(number_plane.coords_to_point(3.3,2.2)))
                                 ))
        self.play(Wait(3))

        self.play(Create(Line([2,3,0], [2.5,2.5,0], color=GREEN_A)))
        self.play(Wait(1))

        self.play(FadeOut(mRotA, mRotC, mProjectB))
        self.play(Write(finalTrans.move_to(mProjectB)))
        self.play(Wait(3))

