########################
# Beziers intersection #
########################

from math import *

class bezierIntersection:
    def __init__(self):
        pass
        
    def calc_bez_length(self,((x1,y1),(x2,y2),(x3,y3),(x4,y4)),Delta_x_input_max):
        
        """
        ( ((x1,y1),(x2,y2),(x3,y3),(x4,y4)),Delta_x_input_max)
        calculate the length of a bezier, and the t fraction needed to calulate the input x precision
        example  for Delta_x_input_max = 0.001   and a bezier with a length = 100 we will have a Delta_t_bez_cal = 0.00001
        return a tuple (x,y)
        """
    
        Delta_t_bez_cal=0.1
    
        x_strt=x1
        y_strt=y1
    
        l_bex=0
    
        t_bez_cal=0

        cx = 3*(x2 - x1)
        bx = 3*(x3 - x2) - cx
        ax = x4 - x1 - cx - bx
        cy = 3*(y2 - y1)
        by = 3*(y3 - y2) - cy
        ay = y4 - y1 - cy - by

        while True:
            if t_bez_cal >= 1:
                break
            else:
                pass   
                
            t_bez_cal3=t_bez_cal*t_bez_cal*t_bez_cal
            t_bez_cal2=t_bez_cal*t_bez_cal
        
            x_bez_cal = ax*(t_bez_cal3) + bx*(t_bez_cal2) + cx*t_bez_cal + x1
            y_bez_cal = ay*(t_bez_cal3) + by*(t_bez_cal2) + cy*t_bez_cal + y1

            t_bez_cal=t_bez_cal+Delta_t_bez_cal
        
            bez_dis=hypot((x_bez_cal-x_strt),(y_bez_cal-y_strt))

            x_strt=x_bez_cal
            y_strt=y_bez_cal

            l_bex=l_bex+bez_dis
        
        Delta_t_bez_cal= Delta_x_input_max / l_bex            
        return l_bex, Delta_t_bez_cal
    
    def calc_bez_xy (self,X1,((x1,y1),(x2,y2),(x3,y3),(x4,y4)),Delta_x_input_max,Delta_t_bez_cal):
        
        """(X1,((x1,y1),(x2,y2),(x3,y3),(x4,y4)),Delta_x_input_max,Delta_t_bez_cal)
        calculate y coordinate on a bezier curve given a x value
        Delta_x_input_max is the precision required es (0.001) 
        Delta_t_bez_cal is the scan precision es (0.001) 
        if Delta_t_bez_cal is == a sutable number will be calculated using calc_bez_length """
        
        if Delta_t_bez_cal== None:
            calc_bez= self.calc_bez_length (((x1,y1),(x2,y2),(x3,y3),(x4,y4)),Delta_x_input_max)
            Delta_t_bez_cal=calc_bez[1]
        
        x_input=X1        
        t_bez_cal=0
    
        cx = 3*(x2 - x1)
        bx = 3*(x3 - x2) - cx
        ax = x4 - x1 - cx - bx
        cy = 3*(y2 - y1)
        by = 3*(y3 - y2) - cy
        ay = y4 - y1 - cy - by
    
        Delta_x_input_eff=float("inf")

        while True:
            if t_bez_cal>1 or Delta_x_input_eff <= Delta_x_input_max:
                break
            else:
                pass
                                  
            t_bez_cal=t_bez_cal+Delta_t_bez_cal    
            t_bez_cal3=t_bez_cal*t_bez_cal*t_bez_cal
            t_bez_cal2=t_bez_cal*t_bez_cal
            
            x_bez_cal = ax*(t_bez_cal3) + bx*(t_bez_cal2) + cx*t_bez_cal + x1
            y_bez_cal = ay*(t_bez_cal3) + by*(t_bez_cal2) + cy*t_bez_cal + y1

            Delta_x_input_eff=fabs(x_input-x_bez_cal)
    
        if t_bez_cal>1.001:            
            return None
        else: 
            return x_input, y_bez_cal
   
    def calc_bez_t (self,t_bez_cal,((x1,y1),(x2,y2),(x3,y3),(x4,y4))):
        
        """
        (t_bez_cal,((x1,y1),(x2,y2),(x3,y3),(x4,y4)))
        calculate the x,y on a bezier for a given t 0<1
        return a tuple (x,y)
        """
        
        if t_bez_cal<0 or t_bez_cal>1.001:            
            return None
    
        cx = 3*(x2 - x1)
        bx = 3*(x3 - x2) - cx
        ax = x4 - x1 - cx - bx
        cy = 3*(y2 - y1)
        by = 3*(y3 - y2) - cy
        ay = y4 - y1 - cy - by
    
        t_bez_cal3=t_bez_cal*t_bez_cal*t_bez_cal
        t_bez_cal2=t_bez_cal*t_bez_cal
        x_bez_cal = ax*(t_bez_cal3) + bx*(t_bez_cal2) + cx*t_bez_cal + x1
        y_bez_cal = ay*(t_bez_cal3) + by*(t_bez_cal2) + cy*t_bez_cal + y1
 
        return x_bez_cal,y_bez_cal

    def calc_int_bez (self,((x1,y1),(x2,y2),(x3,y3),(x4,y4)),((X1,Y1),(X2,Y2),(X3,Y3),(X4,Y4))):
        """
        (((x1,y1),(x2,y2),(x3,y3),(x4,y4)),((X1,Y1),(X2,Y2),(X3,Y3),(X4,Y4))) calculate interception between twobezier curves xI yIV ancor1 manipulator1 manipulator2 ancor2 first curve XI YIV ancor1 manipulator1 manipulator2 ancor2 second curve if you are using a bezier to describe a segment please insert it as the first curvererturn a tuple x,y value of the interception 
        """
        t=0
        
        #start the calculation on the first curve
        calc_bez_xyl_1= self.calc_bez_length (((x1,y1),(x2,y2),(x3,y3),(x4,y4)),0.1)
        Delta_t_incremental=calc_bez_xyl_1[1]
    
        calc_bez_xyl_2= self.calc_bez_length (((X1,Y1),(X2,Y2),(X3,Y3),(X4,Y4)),0.1)
        Delta_t_bez_cal=calc_bez_xyl_2[1]
    
        coor= self.calc_bez_t (t,((x1,y1),(x2,y2),(x3,y3),(x4,y4)))
    
        coor2= self.calc_bez_xy (coor[0],((X1,Y1),(X2,Y2),(X3,Y3),(X4,Y4)),0.1, Delta_t_bez_cal)

        delta_ins=hypot(coor[0]-coor2[0],coor[1]-coor2[1])
        delta_ins_pre=delta_ins

        delta_t_bez=delta_ins_pre/100000
    
        while True:
            if t  > 1 or delta_ins > delta_ins_pre :
                break
            else:
                pass
            
            if delta_t_bez>0.0005:
                delta_t_bez=delta_ins_pre/100000
            else:
                delta_t_bez=0.0005
            t_pre=t
            t=t+delta_t_bez
            delta_ins_pre=delta_ins
            coor= self.calc_bez_t (t,((x1,y1),(x2,y2),(x3,y3),(x4,y4)))
    
            coor2= self.calc_bez_xy (coor[0],((X1,Y1),(X2,Y2),(X3,Y3),(X4,Y4)),0.1, Delta_t_bez_cal)
            delta_ins=hypot(coor[0]-coor2[0],coor[1]-coor2[1])
    
        coor= self.calc_bez_t (t_pre,((x1,y1),(x2,y2),(x3,y3),(x4,y4)))
        X_out=coor[0]
        Y_out=coor[1]
    
        if t > 1.01 or delta_ins >10 :
            return None
        else:
            return X_out,Y_out