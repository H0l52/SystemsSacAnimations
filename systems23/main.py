from manim import *
from manim.animation.transform import Transform
from manim.mobject.mobject import Mobject
import random

from manim_voiceover import VoiceoverScene
from manim_voiceover.services.recorder import RecorderService

class RotateToDec(Rotate):
    def __init__(self, mobject: DecimalNumber, mobject2: Mobject, initval, endval, **kwargs):
        self.start = initval
        self.end = endval

        self.original = mobject

        super().__init__(mobject2, **kwargs)

    def lerp(a: float, b: float, t: float) -> float:
            return (1 - t) * a + t * b
    
    def interpolate_mobject(self, alpha: float) -> None:
        
        actual_alpha = self.rate_func(alpha)
        self.original.set_value(RotateToDec.lerp(self.start, self.end, actual_alpha))

        return super().interpolate_mobject(alpha)

class IonSpinView(VoiceoverScene):
    def construct(self):
        self.set_speech_service(RecorderService())

        arcs = VGroup(
            Arc(angle=PI, start_angle=3*PI/2, arc_center=[-8,0,0], radius=8, stroke_opacity=0.4),
            Arc(angle=PI, start_angle=3*PI/2, arc_center=[-8,0,0], radius=6, stroke_opacity=0.4),
            Arc(angle=PI, start_angle=3*PI/2, arc_center=[-8,0,0], radius=4, stroke_opacity=0.4),
            Arc(angle=PI, start_angle=3*PI/2, arc_center=[-8,0,0], radius=2, stroke_opacity=0.4),
        )
        
        with self.voiceover("But the Qubit cannot just be \"read\" or \"written to\" in the conventional manner. A Qubit must be completely isolated to function correctly, as it is extremely fragile <bookmark mark='A'/>and it will decohere (destroy its state) to any amount of noise (interference), including reading the value from the Qubit. "):
            self.play(Create(arcs, lag_ratio=0.2))

            circle = Circle(color=BLUE)
            circle.set_fill(BLUE, opacity=0.9)
            self.play(GrowFromCenter(circle))

            arrow = Arrow(start=[0,2,0], end=[0,-2,0], color=YELLOW)
            
            bitVal = DecimalNumber(0, font_size=96,
                                num_decimal_places=1)
            bitVal.shift([4,0,0])
            
            self.play(GrowArrow(arrow))
            #self.play(Write(bitVal))

            self.wait_until_bookmark("A")
            
            self.play(Rotate(arrow, angle=48*PI, run_time=5))

        temp = Tex(r"Temp 1 K", font_size=96)
        temp.shift([4,2,0])

        lowtemp = Tex(r"Temp 0.01 K", font_size=96)
        lowtemp.shift([4,2,0])


        with self.voiceover("So a qubit has to be trapped in a material of extremely low temperature (only a few hundredths of a degree above 0 kelvin) to ensure it doesn't automatically decohere, and it must be surrounded by a material of no magnetic resonance, <bookmark mark='A'/>or no nuclear spin. "):
            self.play(AnimationGroup(
                    Write(temp),
                    #Write(bitVal)
                ))
            self.wait_until_bookmark("A")
            self.play(FadeOut(arcs, circle, arrow, temp))
        # Silicon 28
        with self.voiceover("A good example is silicon-28, which is being used by researchers around the world with electrons on electronegative ions to produce a qubit"):
            self.play(Wait(3))

        with self.voiceover("The electrons on the ion spin around the ion within the magnetic fields, <bookmark mark='A'/>and when they have maximum energy, they can spin around in the opposite <bookmark mark='B'/> direction."):
            self.play(FadeIn(arcs, circle, arrow, temp))
            
            self.play(AnimationGroup(
                    Transform(temp,lowtemp),
                    Write(bitVal)
                ))

            self.wait_until_bookmark("A")
            self.play(RotateToDec(bitVal, arrow, 0, 1))

            self.play(RotateToDec(bitVal, arrow, 1, 0))

            self.wait_until_bookmark("B")
            self.play(FadeOut(arcs, circle, arrow, bitVal, temp))

        # Reading QUBITS

        with self.voiceover(""" To read a qubit's state differs with the approach taken, but in the case of electron qubits, 
                            the qubit must be embedded in proximity to a transistor. As the transistor is pulsed, 
                            if the qubit is in an up spin (or high energy state of >0.5), it will have enough energy to 
                            <bookmark mark='A'/> jump from its atom to the stream of flowing electrons, creating a spike in current. 
                            In all other circumstances <bookmark mark='B'/>it will produce no effect. """):
            cim = Circle(radius=1, color=GREEN_B).set_fill(GREEN_C, 0.8)
            cir1 = Circle(radius=1.4, color=WHITE, stroke_opacity=0.5)
            cir2 = Circle(radius=1.8, color=WHITE, stroke_opacity=0.5)
            cir3 = Circle(radius=2.2, color=WHITE, stroke_opacity=0.5)
            ci = VGroup(cim, cir1, cir2, cir3).move_to(LEFT*3)
            cie = Circle(radius=0.25, color=BLUE_B).set_fill(BLUE_C, 0.8).next_to(cim, UP*0.2+RIGHT*2)
            self.play(Create(VGroup(ci,cie), lag_ratio=0.2))
            self.play(Rotate(cie, angle= 2*PI, about_point=cim.get_center()))

            ele = Circle(radius = 0.25, color= RED_B).set_fill(RED_C, 0.8).move_to([8,10,0])
            
            a = VGroup()
            for x in range(30):
                a.add(ele.copy().shift([random.uniform(-4, 0), random.uniform(-4, 4), 0]))

            self.wait_until_bookmark("A")
            self.play(AnimationGroup(
                ApplyMethod(a.shift, [0,-10,0], rate_func=rate_functions.ease_in_cubic),
                ApplyMethod(cie.shift, [6,0,0], rate_func=rate_functions.ease_in_cubic)
                ))
            
            self.play(AnimationGroup(
                ApplyMethod(a.shift, [0,-10,0], rate_func=rate_functions.ease_out_cubic),
                ApplyMethod(cie.shift, [0,-10,0], rate_func=rate_functions.ease_out_cubic)
                ))

            self.wait_until_bookmark("B")
            self.play(FadeOut(cie, ci, a, ele))


        # back to spin view
        with self.voiceover(""" To write to a qubit requires an electromagnetic 
                            wave in tune with the resonance 
                            hertz of the Qubit, pulsed for long enough to absorb enough energy to 
                            <bookmark mark='A'/>flip directions on the magnetic field. 
                            The wrong frequency (hertz), and the Qubit will not be able to 
                            absorb the information."""):
            self.play(FadeIn(arcs, circle, arrow, bitVal, temp))

            self.play(AnimationGroup(
                FadeIn(Arc(arc_center=[-6,0,0],radius=2, angle=PI, color=RED,)),
                FadeIn(Arc(arc_center=[-2,0,0],radius=2, angle=PI, start_angle=PI, color=RED)),
                ))

            self.wait_until_bookmark("A")
            self.play(RotateToDec(bitVal, arrow, 0, 1))
            
            self.play(Wait(2))
        self.play(Wait(2))
        #self.play(FadeOut(circle))

class BirdAnalogy1(VoiceoverScene):
    def construct(self):
        self.set_speech_service(RecorderService())
        #
        # Create the window, eye and bird
        #
        
        bird = SVGMobject("bird.svg", stroke_color=BLUE_C, stroke_width=8, width=2.5, height=2.5)
        
        window = VGroup(Square(side_length=4).shift(DOWN),
            Square(side_length=4).shift(UP))
        
        
        upperLid = Line(start=[6,0,0], end=[5.5,1,0], color=YELLOW_B)
        lowerLid = Line(start=[6,0,0], end=[5.5,-1,0], color=YELLOW_B)
        pupil = Angle(upperLid, lowerLid,radius=0.5, color=BLUE_A)
        youtext = Tex("You", font_size=32, color=BLUE_A).move_to([5.75,-1.5,0])
        eye = VGroup(pupil,upperLid,lowerLid, youtext)

        with self.voiceover(text="Think of the qubit like a bird seen through a window.") as tracker:
            self.play(AnimationGroup(
                Create(window,lag_ratio=0)),
                Create(eye, lag_ratio=0)
                )
            
            self.play(Create(bird))

        #
        # Whilst you aren't looking, 
        # the bird could be anywhere beyond the window, in any position.
        #
        with self.voiceover(text="Whilst you aren't looking, the bird could be anywhere beyond the window, in any position."):
            self.play(FadeOut(bird))
        
        self.play(FadeIn(bird))


        #
        # When you look through the window 
        # you can see if the bird is looking towards you (a 1) or away (a 0).
        # 

        zbit = Integer(0, font_size=96)
        zbit.shift([-4,0,0])

        obit = Integer(1, font_size=96)
        obit.shift([4,0,0])

        with self.voiceover(text="When you look through the window,  you can see if the bird is looking towards you (a 1) <bookmark mark='A'/>or away (a 0)."):
            self.play(Write(obit))
            self.wait_until_bookmark("A")
            self.play(AnimationGroup(
                Rotate(bird, axis=[0,1,0], angle=PI),
                Write(zbit)
                ))

        #
        # But you cannot observe constantly 
        # - you are required to blink, turn away, get food and live your daily life
        #

        qmark = Tex("?", font_size=96)

        with self.voiceover(text="But you cannot observe constantly - <bookmark mark='A'/>you are required to blink, turn away, get food and live your daily life"):
            self.play(FadeOut(obit,zbit,bird))
            self.wait_until_bookmark("A")
            self.play(FadeIn(qmark))


        #
        # After profiling the bird for many years you have a 
        # probability representing the chance of the bird facing you or not,
        # for example there's an 87% chance the bird is facing the window. 
        #
        b2 = bird.copy().rotate(PI, [0,1,0])
        obit.set_color(RED)

        with self.voiceover("After profiling the bird for many years you have a <bookmark mark='A'/>probability representing the chance of the bird facing you or not,<bookmark mark='B'/>for example there's an 87% chance the bird is facing the window. This represents the second piece of information, a probability of position. "):
            self.play(AnimationGroup(FadeOut(qmark), FadeIn(b2, obit, zbit)))

            window2 = VGroup(window.copy(), bird, zbit.copy().set_color(RED), obit.copy().set_color(WHITE))

            window.add(b2, obit, zbit)

            self.wait_until_bookmark("A")
            self.play(window.animate.move_to(LEFT*4 + UP*2).scale(0.6))

        
            window2.move_to(LEFT*4 + DOWN*2).scale(0.6)

            self.play(FadeIn(window2))

            chanceUpper = MathTex(r"87 \%", font_size=64, color=GREEN_B).move_to(LEFT*0.5 + UP*2)
            chanceLower = MathTex(r"13 \%", font_size=64, color=GREEN_A).move_to(LEFT*0.5 + DOWN*2)
            self.wait_until_bookmark("B")
            self.play(FadeIn(chanceUpper, chanceLower))

        #
        # Together the state (facing or not, 1 or 0) and the probability of being in that state (1.0, 0.5, 0.87, etc.) 
        # represent the two pieces of information a qubit can give when observed.
        #
        with self.voiceover("Together the state (facing or not, 1 or 0) and the probability of being in that state<bookmark mark='A'/> (1.0, 0.5, 0.87, etc.) represent the two pieces of information a qubit can give when observed."):
            self.play(FadeOut(eye, chanceLower, window2))

            window.add(chanceUpper)
            self.play(window.animate.move_to(LEFT*2).scale(0.8/0.6))

            self.play(obit.animate.set_color(WHITE))
            self.play(zbit.animate.set_color(RED))
            self.play(zbit.animate.set_color(WHITE))

            self.play(chanceUpper.animate.shift(RIGHT*2))

            self.wait_until_bookmark("A")
            self.play(Transform(chanceUpper, DecimalNumber(1, font_size=64).move_to(chanceUpper)))
            self.play(Transform(chanceUpper, DecimalNumber(0.5, font_size=64).move_to(chanceUpper)))
            self.play(Transform(chanceUpper, DecimalNumber(0.87, font_size=64).move_to(chanceUpper)))
            self.play(Transform(chanceUpper, MathTex(r"87 \%", font_size=64).move_to(chanceUpper)))

        # 
        # This means that at any one instance of time, 
        # a qubit can provide 2 pieces of information rather than the singular ...
        # 
        window -= chanceUpper
        with self.voiceover("This means that at any one instance of time, a qubit can provide <bookmark mark='A'/>2 pieces of information  rather than the singular piece of information a classical bit provides."):
            self.wait_until_bookmark("A")
            self.play(Flash(window, flash_radius=2, 
                            line_length=0.5, line_stroke_width = 5))
            self.play(Flash(chanceUpper, flash_radius=1,
                            line_length=0.25, line_stroke_width = 5))
        
        #
        # In a quantum level, the qubit’s state is often represented 
        # by its spin direction,
        # where magnetic poles determine the 1 or 0 state.
        #
        with self.voiceover(r"In a quantum level, <bookmark mark='A'/>the qubit's state is often represented by its spin direction around a magnetic field where magnetic poles determine the 1 or <bookmark mark='B'/>0 state."):
            self.play(AnimationGroup(
                Uncreate(window, lag_ratio=0)),
                Unwrite(chanceUpper)
            )

            magField = VGroup(
                Arc(angle=PI, start_angle=3*PI/2, radius=8, stroke_opacity=0.4),
                Arc(angle=PI, start_angle=3*PI/2, radius=6, stroke_opacity=0.4),
                Arc(angle=PI, start_angle=3*PI/2, radius=4, stroke_opacity=0.4),
                Arc(angle=PI, start_angle=3*PI/2, radius=2, stroke_opacity=0.4),
            )

            oppoMagField = magField.copy().flip()
            magField.shift(LEFT*8)

            self.wait_until_bookmark("A")
            self.play(FadeIn(magField, oppoMagField))

            electron = Circle(radius=2, color=BLUE).set_fill(BLUE_C, 0.5)
            arrowdirection = Arrow(start=[0,-2,0], end=[0,2,0], color=YELLOW)
            self.play(AnimationGroup(
                Create(electron)),
                GrowArrow(arrowdirection)
                )
            self.wait_until_bookmark("B")
            self.play(Rotate(arrowdirection, angle=PI))
            self.play(FadeOut(magField, oppoMagField, electron, arrowdirection))

class BirdAnalogy2(VoiceoverScene):
    def construct(self):
        self.set_speech_service(RecorderService())
        #
        # So when 2 qubits, or birds, are observed together,
        # a new set of information is present.
        #
        birdBase = SVGMobject("bird.svg", stroke_color=BLUE_C, stroke_width=2, width=1.75, height=1.75).shift([0,0.2,0])
        integerBase = Integer(1, color=RED_C)
        birdOnSpace = VGroup(birdBase, integerBase)

        birdOffSpace = birdOnSpace.copy()
        birdOffSpace[0].flip()
        birdOffSpace[1].set_value(0)
        birdOffSpace[1].set_color(WHITE)

        stateS = MobjectTable(
            [
                [birdOffSpace.copy(),birdOffSpace.copy()],
                [birdOffSpace.copy(),birdOnSpace.copy()],
                [birdOnSpace.copy(),birdOffSpace.copy()],
                [birdOnSpace.copy(),birdOnSpace.copy()],
            ]
        )

        stateSwP = MobjectTable(
            [
                [MathTex(r"\alpha", font_size=64, color=BLUE_B), birdOffSpace.copy(),birdOffSpace.copy()],
                [MathTex(r"\beta", font_size=64, color=BLUE_B), birdOffSpace.copy(),birdOnSpace.copy()],
                [MathTex(r"\gamma", font_size=64, color=BLUE_B), birdOnSpace.copy(),birdOffSpace.copy()],
                [MathTex(r"\delta", font_size=64, color=BLUE_B), birdOnSpace.copy(),birdOnSpace.copy()],
            ]
        )

        with self.voiceover("So when 2 qubits, or birds, are observed together, a new set of information is present. You could either have 0,0 0,1 1,0 or 1,1 in a conventional computer. "):
            self.play(FadeIn(stateS))

        #
        # But in a quantum computer, to represent the state as a whole, 
        # 4 coefficients representing the probabilities of each individual 
        # state of the two qubits must be taken into consideration.
        #

        with self.voiceover("But in a quantum computer, to represent the state as a whole, <bookmark mark='A'/>4 coefficients representing the probabilities of each individual state of the two qubits must be taken into consideration. "):
            self.wait_until_bookmark("A")
            self.play(Transform(stateS, stateSwP))

        #
        # So at any one time, we now have 4 bits worth of information,
        # the relevant probabilities of each of the classical bit states. 
        #
        with self.voiceover("So at any one time, we now have 4 bits worth of information, the relevant probabilities of each of the classical bit states."):
            self.play(Flash(stateSwP[0][0], flash_radius=0.3))
            self.play(Flash(stateSwP[0][3], flash_radius=0.3))
            self.play(Flash(stateSwP[0][6], flash_radius=0.3))
            self.play(Flash(stateSwP[0][9], flash_radius=0.3))

        #
        # In an exponential sense, scaling up to three qubits would...
        #

        with self.voiceover("In an exponential sense, scaling up to three qubits would require 8 numbers representing the state of the system, despite the 3 classical bits worth of data."):
            self.play(FadeOut(stateS))

        equatGen = MathTex(r"N_{qubits}", font_size=96)
        equatGen2 = MathTex(r"Data_{\:qubits} = 2^{N_{bits}}", font_size=96)

        with self.voiceover("Generalising, we find that the amount of <bookmark mark='A'/>information N qubits represent is <bookmark mark='B'/>2^N classical bits."):
            self.wait_until_bookmark("A")
            self.play(Write(equatGen))
            self.wait_until_bookmark("B")
            self.play(Transform(equatGen,equatGen2))

        #
        # So with 300 qubits, we can represent 2^300 or 2.03703598*10^90
        # pieces of information,
        # which is more than the amount of atoms in the universe (10^82).
        #

        with self.voiceover("So with 300 qubits, we can represent 2^300 or <bookmark mark='A'/>2.03703598*10^90 pieces of information, which is more than the amount of atoms in the universe (10^82)."):
            self.play(equatGen.animate.shift(UP*1.5))

            useEquat = MathTex(r"Data_{\:300} = 2^{300}", font_size=96)
            useSolvedEquat = MathTex(r"Data_{\:300} = 2.03703598 \cdot 10^{90}", font_size=96)

            self.play(Write(useEquat))
            self.wait_until_bookmark("A")
            self.play(Transform(useEquat, useSolvedEquat))

        #
        # We cannot measure the probabilities ...
        # 
        with self.voiceover("We cannot measure the probabilities during our calculations of Qubits, and when observing the bit, it must fall into the position represented by a classical state, <bookmark mark='A'/>so despite having 8 pieces of information on the position of birds out of the window, only 3 bits will ever be interpreted, the actual facing directions."):
            self.play(FadeOut(useEquat, equatGen))

            birdOffSpace.scale(0.4)
            birdOnSpace.scale(0.4)

            triobirdsS = MobjectTable(
                [
                    [MathTex(r"\alpha", font_size=32, color=BLUE_B), birdOffSpace.copy(),birdOffSpace.copy(),birdOffSpace.copy()],
                    [MathTex(r"\beta", font_size=32, color=BLUE_B), birdOffSpace.copy(),birdOffSpace.copy(),birdOnSpace.copy()],
                    [MathTex(r"\gamma", font_size=32, color=BLUE_B), birdOffSpace.copy(),birdOnSpace.copy(),birdOffSpace.copy()],
                    [MathTex(r"\delta", font_size=32, color=BLUE_B), birdOffSpace.copy(),birdOnSpace.copy(),birdOnSpace.copy()],
                    [MathTex(r"\epsilon", font_size=32, color=BLUE_B), birdOnSpace.copy(),birdOffSpace.copy(),birdOffSpace.copy()],
                    [MathTex(r"\zeta", font_size=32, color=BLUE_B), birdOnSpace.copy(),birdOffSpace.copy(),birdOnSpace.copy()],
                    [MathTex(r"\eta", font_size=32, color=BLUE_B), birdOnSpace.copy(),birdOnSpace.copy(),birdOffSpace.copy()],
                    [MathTex(r"\theta", font_size=32, color=BLUE_B), birdOnSpace.copy(),birdOnSpace.copy(),birdOnSpace.copy()],
                ], v_buff=0.4, h_buff=0.8
            )

            triobirds = MobjectTable(
                [
                    [birdOffSpace.copy(),birdOffSpace.copy(),birdOffSpace.copy()],
                    [birdOffSpace.copy(),birdOffSpace.copy(),birdOnSpace.copy()],
                    [birdOffSpace.copy(),birdOnSpace.copy(),birdOffSpace.copy()],
                    [birdOffSpace.copy(),birdOnSpace.copy(),birdOnSpace.copy()],
                    [birdOnSpace.copy(),birdOffSpace.copy(),birdOffSpace.copy()],
                    [birdOnSpace.copy(),birdOffSpace.copy(),birdOnSpace.copy()],
                    [birdOnSpace.copy(),birdOnSpace.copy(),birdOffSpace.copy()],
                    [birdOnSpace.copy(),birdOnSpace.copy(),birdOnSpace.copy()],
                ], v_buff=0.4, h_buff=0.8
            )

            self.play(FadeIn(triobirdsS))
            self.wait_until_bookmark("A")
            self.play(Transform(triobirdsS,triobirds))