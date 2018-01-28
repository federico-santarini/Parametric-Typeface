'''
Isia Urbino
a.a. 2013/14

Tecniche tipografiche I
professore:
Luciano Perondi

Federico
Santarini
matricola n°
1121
'''

import robofab.world
import math
from robofab.world import  * 
from math import  * 
from tools import tools
from tools import bezIntersection
from robofab.misc.bezierTools import splitCubic


## Class constructor
tools = tools.Tools()
bezTool = bezIntersection.bezierIntersection()

# interruttori
on = True
off = False

## Costanti
# Altezza x
xht_nom = 500

## Variabili indipendenti (valori nominali)

# Espansione
exp_nom = .6

# peso
wgt_nom = .18

# SquadraturaGenerale
gsq_nom = .6

# ModulazioneSquadratura
mod_nom = 1

# Rapporto n/o
nor_nom = .9

# Monolinearità
mon_nom = .65

# Ascendenti e Discendenti
asc = .45
dsc = .45

# Raccordo
raccordoUpp_nom = 1.2
raccordoDwn_nom = .6

# Innesto
inn_nom = .8

# Tacco
innSpr_nom = 0
dstSpr_nom = tools.par (wgt_nom , 0.04 , 0.001 , 0.18 , 0.01 , 0.3 , 0.02)
gapSpr_nom = 0.1

if gapSpr_nom < 0:
    gapSpr_nom = 0
else:
    gapSpr_nom

if gapSpr_nom > wgt_nom - dstSpr_nom:
    gapSpr_nom = wgt_nom - dstSpr_nom
else:
    gapSpr_nom


# Spaziatura
spc_nom = .14


## Variabili dipendenti (valori effettivi)

# Altezza x
xht_eff = xht_nom

# Peso
wgt_eff = wgt_nom

# SquadraturaGenerale
gsq_eff = gsq_nom

# ModulazioneSquadratura
mod_eff = mod_nom

# Rapporto n/o
nor_eff = nor_nom

# Espansione
exp_eff = 2 * exp_nom / (nor_nom + 1)

# Monolinearità
mon_eff = mon_nom

# Overshooting
ovs_eff = 8

# Spessore Aste: verticali, orizzontali
tsv_eff = wgt_eff * xht_eff
tsh_eff = tsv_eff * mon_eff

# Spessore Curve verticali, orizzontali
tcv_eff = tsv_eff
tch_eff = tcv_eff * mon_eff

# Squadratura Interna
sqi_eff = gsq_eff * mod_eff
if sqi_eff > 1:
    sqi_eff = 1
else:
        sqi_eff

# Squadratura Esterna
delta_gsq_eff_gsq_eff = tools.lin (gsq_eff , 0.54 , 0 , 1 , -0.08)
sqe_eff = gsq_eff + delta_gsq_eff_gsq_eff

# Ascendenti/Discendenti
asc_eff  =  asc * xht_eff
dsc_eff  =  dsc * xht_eff

# Raccordo
raccordoUpp_eff = raccordoUpp_nom * xht_eff
raccordoDwn_eff = raccordoDwn_nom * xht_eff

# Innesti
inn_eff = inn_nom

# Tacco (spur)
delta_innSpr_wgt = tools.par (wgt_nom , 0.04 , 0.145 , 0.18 , 0.147 , 0.3 , 0.158)
delta_innSpr_gsq = tools.lin (gsq_nom , 0.54 , 0.008 , 1 , -0.06)

innSpr_eff = innSpr_nom * (delta_innSpr_wgt + delta_innSpr_gsq) * xht_eff
dstSpr_eff = dstSpr_nom * xht_eff
gapSpr_eff = gapSpr_nom * xht_eff

# Spaziatura
deltaSpc_wgt = tools.par (wgt_eff , 0.4 , -0.05 , 0.18 , 0 , 0.04 , 0.04)
deltaSpc_gsq = tools.lin (gsq_eff , 0.54 , -0.08 , 1 , -0.02)
#deltaSpc_nor =
#deltaSpc_exp =

spc_eff = spc_nom + deltaSpc_wgt #deltaSpc_nor + deltaSpc_exp
spcStm_eff = xht_eff * spc_eff
spcCrv_eff = (spc_eff + deltaSpc_gsq) * xht_eff

mrgStm_eff = spcStm_eff
mrgCrv_eff = spcCrv_eff



# Glyph: o (unicode: 006F)
def drawing_o():
    
    # Calling glyph
    font = CurrentFont ()
    glyph = font['o']
    glyph.clear()
    
    
    ## Variabili Locali [o]
    #Larghezza
    wdt_o = exp_eff * xht_eff
    # Monolinearità
    estremo_mon_mon = tools.lin (mon_eff , 0 , 0.15 , 1 , 0)
    mon_o = tools.lin(mon_eff,1,1,0.01,0.2)
    # Spessore Curve Orizzontali
    tch_o = tcv_eff * mon_o

    
    ### UpperLeft
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (0) , (xht_eff + ovs_eff)
    hnd_1 = ( ((-wdt_o/2 - tcv_eff/2) * sqe_eff)) , (xht_eff + ovs_eff)
    hnd_2 = (-wdt_o/2 - tcv_eff/2) , ((xht_eff/2) + ((+xht_eff/2 + ovs_eff) * sqe_eff))
    pnt_2 = (-wdt_o/2 - tcv_eff/2) , (xht_eff/2)
    
    #int
    pnt_3 = (-wdt_o/2 + tcv_eff/2) , (xht_eff/2)
    hnd_3 = (-wdt_o/2 + tcv_eff/2) , (xht_eff/2) + (((xht_eff/2 + ovs_eff - tch_o) * sqi_eff))
    hnd_4 = ((-wdt_o/2 + tcv_eff/2) * sqi_eff) , (xht_eff - tch_o + ovs_eff)
    pnt_4 = (0) , (xht_eff - tch_o + ovs_eff)

    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
              
    ### BottomLeft
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (-wdt_o/2 - tcv_eff/2) , (xht_eff/2)
    hnd_1 = (-wdt_o/2 - tcv_eff/2) , ((xht_eff/2) + ((-xht_eff/2 - ovs_eff) * sqe_eff))
    hnd_2 = (((-wdt_o/2 - tcv_eff/2) * sqe_eff)) , (-ovs_eff)
    pnt_2 = (0) , (-ovs_eff)
    
    #int
    pnt_3 = (0) , (tch_o - ovs_eff)
    hnd_3 = ( ((-wdt_o/2 + tcv_eff/2) * sqi_eff)) , (tch_o - ovs_eff)
    hnd_4 = (-wdt_o/2 + tcv_eff/2) , ((xht_eff/2) + ((-xht_eff/2 - ovs_eff + tch_o) * sqi_eff))
    pnt_4 =  (-wdt_o/2 + tcv_eff/2) , (xht_eff/2)

    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
              
    ### BottomRight
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (0) , (-ovs_eff)
    hnd_1 = (((wdt_o/2 + tcv_eff/2) * sqe_eff)) , (-ovs_eff)
    hnd_2 = (wdt_o/2 + tcv_eff/2) , ((xht_eff/2) + ((-xht_eff/2 - ovs_eff) * sqe_eff))
    pnt_2 = (wdt_o/2 + tcv_eff/2) , (xht_eff/2)
    
    #int
    pnt_3 = (wdt_o/2 - tcv_eff/2) , (xht_eff/2)
    hnd_3 = (wdt_o/2 - tcv_eff/2) , ((xht_eff/2) + ((-xht_eff/2 - ovs_eff + tch_o) * sqi_eff))
    hnd_4 = ( ((wdt_o/2 - tcv_eff/2) * sqi_eff)) , (+tch_o - ovs_eff)
    pnt_4 = (0) , (tch_o - ovs_eff)

    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
              
    ### UpperRight
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (wdt_o/2 + tcv_eff/2) , (xht_eff/2)
    hnd_1 = (wdt_o/2 + tcv_eff/2) , ((xht_eff/2) + ((xht_eff/2 + ovs_eff) * sqe_eff))
    hnd_2 = (((wdt_o/2 + tcv_eff/2) * sqe_eff)) , ( xht_eff + ovs_eff)
    pnt_2 = (0) , (xht_eff + ovs_eff)
    
    #int
    pnt_3 = (0) , (xht_eff - tch_o + ovs_eff)
    hnd_3 = (((wdt_o/2 - tcv_eff/2) * sqi_eff)) , (xht_eff - tch_o + ovs_eff)
    hnd_4 = (wdt_o/2 - tcv_eff/2) , ((xht_eff/2) + ((xht_eff/2 + ovs_eff - tch_o) * sqi_eff))
    pnt_4 = (wdt_o/2 - tcv_eff/2) , (xht_eff/2)

    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
    
    glyph.leftMargin = mrgCrv_eff 
    glyph.rightMargin = mrgCrv_eff
    
    return glyph



# Glyph: c (unicode: 0063)
def drawing_c():
    
    # Calling glyph
    font = CurrentFont ()
    glyph = font['c']
    glyph.clear()
    
    # To show cutting shape or the cutting glyph set the on-off switchs 'on'
    showCuttingShape = off
    cut = on
    
    ## Variabili Locali [c]
    #Larghezza
    wdt_c = exp_eff * xht_eff
    # Monolinearità
    mon_c = tools.lin(mon_eff,1,1,0.01,0.2)
    # Spessore Curve Orizzontali
    tch_c = tcv_eff * mon_c
    # Raccordo
    x_inc_upp_c = (raccordoUpp_eff)
    y_inc_upp_c = -(raccordoUpp_eff * 2 / inn_eff)

    x_inc_dwn_c = (raccordoDwn_eff)
    y_inc_dwn_c = -(raccordoDwn_eff * 2 / inn_eff)
    # Disallineamento
    delta_dis_c_wgt = tools.lin(wgt_eff,0.1,0,0.45,0.04)
    delta_dis_c_mon = tools.lin(mon_eff,0.01,0,1,0.02)
    dis_c = (delta_dis_c_wgt + delta_dis_c_mon) * xht_eff
    #spessore curva verticale destra
    deltaCurva_c = tools.lin(mon_eff,0.1,0.1,1,1)
    tcv_effRgt_c = tsv_eff * deltaCurva_c
    
    ### Cutting shape
    ## punti di ancoraggio
    gap_upp_c = 0.3
    dst_upp_c = 0.2
    inn_upp_c = 0.2

    gap_dwn_c = 0.35
    dst_dwn_c = 0.1 
    inn_dwn_c = 0.2

    dstCttShape_upp_c = (dst_upp_c * xht_eff)
    gapCttShape_upp_c = (gap_upp_c * xht_eff)
    innCttShape_upp_c = (inn_upp_c * xht_eff)

    dstCttShape_dwn_c = (dst_dwn_c * xht_eff)
    gapCttShape_dwn_c = (gap_dwn_c * xht_eff)
    innCttShape_dwn_c = (inn_dwn_c * xht_eff)

    if showCuttingShape == True:
        pnt_1 = gapCttShape_upp_c , (xht_eff + ovs_eff)
        pnt_2 = dstCttShape_upp_c , (xht_eff/2 + innCttShape_upp_c)
        pnt_4 = gapCttShape_dwn_c , (-ovs_eff)
        pnt_3 = dstCttShape_dwn_c , (xht_eff/2 - innCttShape_dwn_c)
                                         
        points_list = [(pnt_4) , (pnt_3) , (pnt_2) , (pnt_1)]

        #disegno
        tools.drawPolyByPoints(glyph, points_list)

    
    ### UpperLeft
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (dis_c) , (xht_eff + ovs_eff)
    hnd_1 = (dis_c + ((-wdt_c/2 - tcv_eff/2 - dis_c) * sqe_eff)) , (xht_eff + ovs_eff)
    hnd_2 = (-wdt_c/2 - tcv_eff/2) , ((xht_eff/2) + ((+xht_eff/2 + ovs_eff) * sqe_eff))
    pnt_2 = (-wdt_c/2 - tcv_eff/2) , (xht_eff/2)
    
    #int
    pnt_3 = (-wdt_c/2 + tcv_eff/2) , (xht_eff/2)
    hnd_3 = (-wdt_c/2 + tcv_eff/2) , ((xht_eff/2) + ((xht_eff/2 + ovs_eff - tch_c) * sqi_eff))
    hnd_4 = ((-wdt_c/2 + tcv_eff/2) * sqi_eff) , (xht_eff - tch_c + ovs_eff)
    pnt_4 = 0 , (xht_eff - tch_c + ovs_eff)

    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
              
    ### BottomLeft
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (-wdt_c/2 - tcv_eff/2) , (xht_eff/2)
    hnd_1 = (-wdt_c/2 - tcv_eff/2) , ((xht_eff/2) + ((-xht_eff/2 - ovs_eff) * sqe_eff))
    hnd_2 = ((dis_c) + ((-wdt_c/2 - tcv_eff/2 - dis_c) * sqe_eff)) , (-ovs_eff)
    pnt_2 = (dis_c) , (-ovs_eff)
    
    #int
    pnt_3 = 0 , (tch_c - ovs_eff)
    hnd_3 = (((-wdt_c/2 + tcv_eff/2) * sqi_eff)) , (tch_c - ovs_eff)
    hnd_4 = (-wdt_c/2 + tcv_eff/2) , ((xht_eff/2) + ((-xht_eff/2 - ovs_eff + tch_c) * sqi_eff))
    pnt_4 =  (-wdt_c/2 + tcv_eff/2) , (xht_eff/2)

    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
              
    ### BottomRight
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (dis_c) , (-ovs_eff)
    hnd_1 = (dis_c + ((wdt_c/2 + tcv_effRgt_c/2 - dis_c) * sqe_eff)) , (-ovs_eff)
    hnd_2 = (wdt_c/2 + tcv_effRgt_c/2 + x_inc_dwn_c) , ((xht_eff/2) + ((-xht_eff/2 - ovs_eff) * sqe_eff))
    pnt_2 = (wdt_c/2 + tcv_effRgt_c/2 + x_inc_dwn_c) , (xht_eff/2 - y_inc_dwn_c)
    
    #int
    pnt_3 = (wdt_c/2 - tcv_effRgt_c/2 + x_inc_dwn_c) , (xht_eff/2 - y_inc_dwn_c)
    hnd_3 = (wdt_c/2 - tcv_effRgt_c/2 + x_inc_dwn_c) , ((xht_eff/2) + ((-xht_eff/2 - ovs_eff + tch_c) * sqi_eff))
    hnd_4 = (((wdt_c/2 - tcv_effRgt_c/2) * sqi_eff)) , (+tch_c - ovs_eff)
    pnt_4 = 0 , (tch_c - ovs_eff)
    
    if cut == True:
        cutUp1 = (gapCttShape_dwn_c) , (- ovs_eff)
        cutUp2 = (dstCttShape_dwn_c) , (xht_eff/2 - innCttShape_dwn_c)
        
        interUpExt = bezTool.calc_int_bez ( [cutUp1,cutUp2,cutUp1,cutUp2] ,[pnt_1,hnd_1,hnd_2,pnt_2]  )
        interUpInt = bezTool.calc_int_bez ( [cutUp1,cutUp2,cutUp1,cutUp2] ,[pnt_3,hnd_3,hnd_4,pnt_4]  )
        #interUpExt = (dis_m) , (xht_eff) 
        #interUpInt = (-wdtSpn_dx_m/2 + tsv_eff/2)+wdtSpn_dx_m , 0
        
        cExt = splitCubic (pnt_1,hnd_1,hnd_2,pnt_2,interUpExt[0],False)
        cInt = splitCubic (pnt_3,hnd_3,hnd_4,pnt_4,interUpInt[0],False)
        #ext
        pnt_1 = ((cExt [0])[0])
        pnt_2 = ((cExt [0])[3]) 
        hnd_1 = ((cExt [0])[1])
        hnd_2 = ((cExt [0])[2])
            
        #int
        pnt_3 = ((cInt [1])[0])
        pnt_4 = ((cInt [1])[3])
        hnd_3 = ((cInt [1])[1])  
        hnd_4 = ((cInt [1])[2])
    else:
        pass
    
    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
              
    ### UpperRight
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (wdt_c/2 + tcv_effRgt_c/2 + x_inc_upp_c) , (xht_eff/2 + y_inc_upp_c)
    hnd_1 = (wdt_c/2 + tcv_effRgt_c/2 + x_inc_upp_c) , ((xht_eff/2) + ((xht_eff/2 + ovs_eff) * sqe_eff))
    hnd_2 = ((dis_c) + ((wdt_c/2 + tcv_effRgt_c/2 - dis_c) * sqe_eff)) , ( xht_eff + ovs_eff)
    pnt_2 = (dis_c) , (xht_eff + ovs_eff)
    
    #int
    pnt_3 = 0 , (xht_eff - tch_c + ovs_eff)
    hnd_3 = (((wdt_c/2 - tcv_effRgt_c/2) * sqi_eff)) , (xht_eff - tch_c + ovs_eff)
    hnd_4 = (wdt_c/2 - tcv_effRgt_c/2 + x_inc_upp_c) , ((xht_eff/2) + ((xht_eff/2 + ovs_eff - tch_c) * sqi_eff))
    pnt_4 = (wdt_c/2 - tcv_effRgt_c/2 + x_inc_upp_c) , (xht_eff/2 + y_inc_upp_c)
    
    if cut == True:
        cutUp1 = (gapCttShape_upp_c) , (xht_eff + ovs_eff)
        cutUp2 = (dstCttShape_upp_c) , (xht_eff/2 + innCttShape_upp_c)
        
        interUpExt = bezTool.calc_int_bez ( [cutUp1,cutUp2,cutUp1,cutUp2] ,[pnt_1,hnd_1,hnd_2,pnt_2]  )
        interUpInt = bezTool.calc_int_bez ( [cutUp1,cutUp2,cutUp1,cutUp2] ,[pnt_3,hnd_3,hnd_4,pnt_4]  )
        #interUpExt = (dis_m) , (xht_eff) 
        #interUpInt = (-wdtSpn_dx_m/2 + tsv_eff/2)+wdtSpn_dx_m , 0
        
        cExt = splitCubic (pnt_1,hnd_1,hnd_2,pnt_2,interUpExt[0],False)
        cInt = splitCubic (pnt_3,hnd_3,hnd_4,pnt_4,interUpInt[0],False)
        #ext
        pnt_1 = ((cExt [1])[0])
        pnt_2 = ((cExt [1])[3]) 
        hnd_1 = ((cExt [1])[1])
        hnd_2 = ((cExt [1])[2])
            
        #int
        pnt_3 = ((cInt [0])[0])
        pnt_4 = ((cInt [0])[3])
        hnd_3 = ((cInt [0])[1])  
        hnd_4 = ((cInt [0])[2])
    else:
        pass
    

    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
        
    glyph.leftMargin = mrgCrv_eff 
    glyph.rightMargin = mrgCrv_eff
    
    return glyph



# Glyph: n (unicode: 006E)
def drawing_n():
    # TO DO:
    #    differenziare il movimento delgi innesti interni da quelli esterni (delta??)
    
    
    # Calling glyph
    font = CurrentFont ()
    glyph = font['n']
    glyph.clear()
    
    
    ## Variabili Locali [n]
    # Innesti
    x_inc_upp_n = (raccordoUpp_eff)
    y_inc_upp_n = -(raccordoUpp_eff * 2 / inn_eff)

    # Espansione
    exp_n = exp_eff * nor_eff
    # Larghezza
    wdt_n = exp_n * xht_eff
    # Monilineatià (per neutralizzare la monolinearità a valori alti)
    mon_n=tools.lin(mon_eff,1,1,0.01,0.2)
    # Monilineatià 2 (relazione fra punto medio esterno sinistro e monolinearità
    mon_n2=tools.lin(mon_n,1,mon_n/2,0.01,mon_n)
    # Correzioni otticch sqe_lft/sqe_rgt/sqi_lft/sqi_rgt
    opt_sqe_lft_n=1#.95
    opt_sqe_rgt_n=1#tools.lin(sqe_eff,1,1,0.54,1.5)
    opt_sqi_lft_n=1
    opt_sqi_rgt_n=1#tools.lin(sqi_eff,1,1,0.54,1)
    # Rientro punto medio esterno sinistro
    rtr_extLft_n=mon_n2 * tsv_eff
    #disallineamento punto centale superiore ƒ(wgt,mon,exp)
    delta_dis_n_wgt=tools.lin(wgt_nom,0.01,0,0.45,0.04)
    delta_dis_n_mon=tools.lin(mon_nom,0.01,0,1,0.02)
    delta_n=delta_dis_n_wgt+delta_dis_n_mon
    dis_n=delta_n * xht_eff
    #innalzamento innesti (#inn_eff=ƒ(mon_n , wgt , exp)
    inc_lft_ex = inn_eff * xht_eff
    inc_rgt_ex = inn_eff * xht_eff
    inc_lft_in = inn_eff * xht_eff
    inc_rgt_in = inn_eff * xht_eff
    #spessore curve orizzontali
    tch_n = tcv_eff * mon_n
    


    ### UpperLeft
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (dis_n) , (xht_eff+ovs_eff)
    hnd_1 = (dis_n + ((-wdt_n/2 - tcv_eff/2 + rtr_extLft_n) * sqe_eff * opt_sqe_lft_n)) , (xht_eff + ovs_eff)
    hnd_2 = (-wdt_n/2 + tsv_eff/2 - rtr_extLft_n - x_inc_upp_n) , ((xht_eff/2) + ((xht_eff/2 + ovs_eff) * sqe_eff))
    pnt_2 = (-wdt_n/2 + tsv_eff/2 - rtr_extLft_n - x_inc_upp_n) , (xht_eff/2  + y_inc_upp_n)
    
    #int
    pnt_3 = (-wdt_n/2 + tsv_eff/2 - x_inc_upp_n) , (xht_eff/2  + y_inc_upp_n)
    hnd_3 = (-wdt_n/2+tsv_eff/2 - x_inc_upp_n) , ((xht_eff/2) + ((xht_eff/2 + ovs_eff - tch_n) * sqi_eff))
    hnd_4 = ((-wdt_n/2+tcv_eff/2) * sqi_eff * opt_sqi_lft_n) , (xht_eff + ovs_eff - tch_n)
    pnt_4 = 0 , (xht_eff + ovs_eff - tch_n)

    ### cut
    # Punti di ancoraggio
    interUpExt = (-wdt_n/2 - tsv_eff/2) , (xht_eff) 
    interUpInt = (-wdt_n/2 + tsv_eff/2) , 0
    cExt = splitCubic (pnt_1,hnd_1,hnd_2,pnt_2,interUpExt[0],False)
    cInt = splitCubic (pnt_3,hnd_3,hnd_4,pnt_4,interUpInt[0],False)
    
    #ext
    pnt_1 = ((cExt [0])[0])
    pnt_2 = ((cExt [0])[3]) 
    hnd_1 = ((cExt [0])[1])
    hnd_2 = ((cExt [0])[2])
        
    #int
    pnt_3 = ((cInt [1])[0])
    pnt_4 = ((cInt [1])[3])
    hnd_3 = ((cInt [1])[1])  
    hnd_4 = ((cInt [1])[2])
    

    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
    ### StemLeft
    ## punti di ancoraggio
    
    pnt_1 = (-wdt_n/2 + tsv_eff/2 - dstSpr_eff - gapSpr_eff) , (xht_eff)
    pnt_2 = (-wdt_n/2 - tsv_eff/2) , (xht_eff)
    pnt_3 = (-wdt_n/2 - tsv_eff/2) , 0
    pnt_4 = (-wdt_n/2 + tsv_eff/2) , 0
    pnt_5 = (-wdt_n/2 + tsv_eff/2) , (xht_eff - innSpr_eff)
    pnt_6 = (-wdt_n/2 + tsv_eff/2 - dstSpr_eff) , (xht_eff - innSpr_eff)
                                          
    points_list = [(pnt_1) , (pnt_2) , (pnt_3) , (pnt_4) ,  (pnt_5) , (pnt_6)]

    #disegno
    tools.drawPolyByPoints(glyph, points_list)
 
    ### UpperRight
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (wdt_n/2+tsv_eff/2) , (inc_rgt_ex)
    hnd_1 = wdt_n/2+tsv_eff/2 , (inc_rgt_ex + ((xht_eff + ovs_eff - inc_rgt_ex) * sqe_eff))
    hnd_2 = (dis_n + ((wdt_n/2 + tcv_eff/2 - rtr_extLft_n) * sqe_eff * opt_sqe_rgt_n)) , (xht_eff + ovs_eff)
    pnt_2 = (dis_n) , (xht_eff + ovs_eff)
    
    #int
    pnt_3 =  0 , (xht_eff + ovs_eff - tch_n)
    hnd_3 = ((wdt_n/2 - tcv_eff/2) * sqi_eff * opt_sqi_rgt_n) , (xht_eff + ovs_eff - tch_n)
    hnd_4 = (wdt_n/2 - tsv_eff/2) , (inc_rgt_in + ((xht_eff + ovs_eff - tch_n - inc_rgt_in) * sqi_eff))
    pnt_4 = (wdt_n/2-tsv_eff/2) , (inc_rgt_in)
    
    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
                                          
    ### StemRight
    ## punti di ancoraggio
    pnt_1 = (wdt_n/2 + tsv_eff/2) , (inc_rgt_ex)
    pnt_2 = (wdt_n/2 - tsv_eff/2) , (inc_rgt_in)
    pnt_3 = (wdt_n/2 - tsv_eff/2) , 0
    pnt_4 = (wdt_n/2 + tsv_eff/2) , 0
                                      
    points_list = [(pnt_1) , (pnt_2) , (pnt_3) , (pnt_4)]

    #disegno
    tools.drawPolyByPoints(glyph, points_list)
    
    glyph.leftMargin = mrgStm_eff 
    glyph.rightMargin = mrgCrv_eff

    return glyph



# Glyph: m (unicode: 006D)
def drawing_m():

   
   
    # Calling glyph
    font = CurrentFont ()
    glyph = font['m']
    glyph.clear()
   
    # To show cutting shape or the cutting glyph set the on-off switchs 'on'
    showCuttingShape = on
        cut = off
   
    ## Variabili Locali [m]
    # Innesti
    x_inc_upp_m = (raccordoUpp_eff)
    y_inc_upp_m = -(raccordoUpp_eff * 2 / inn_eff)

    # Espansione
    exp_n = exp_eff * nor_eff
    expSpn_sx = 0.994
    expSpn_dx = 0.983
    # Larghezza
    wdt_n = exp_n * xht_eff
    wdtSpn_sx_m = wdt_n * expSpn_sx
    wdtSpn_dx_m = wdt_n * expSpn_dx

    # Monilineatià (per neutralizzare la monolinearità a valori alti)
    mon_m=tools.lin(mon_eff,1,1,0.01,0.2)
    # Monilineatià 2 (relazione fra punto medio esterno sinistro e monolinearità
    mon_m2=tools.lin(mon_m,1,mon_m/2,0.01,mon_m)
    # Correzioni otticch sqe_lft/sqe_rgt/sqi_lft/sqi_rgt
    opt_sqe_lft_m=1#.95
    opt_sqe_rgt_m=1#tools.lin(sqe_eff,1,1,0.54,1.5)
    opt_sqi_lft_m=1
    opt_sqi_rgt_m=1#tools.lin(sqi_eff,1,1,0.54,1)
    # Rientro punto medio esterno sinistro
    rtr_extLft_m=mon_m2 * tsv_eff
    #disallineamento punto centale superiore ƒ(wgt,mon,exp)
    delta_dis_m_wgt=tools.lin(wgt_nom,0.01,0,0.45,0.04)
    delta_dis_m_mon=tools.lin(mon_nom,0.01,0,1,0.02)
    delta_m=delta_dis_m_wgt+delta_dis_m_mon
    dis_m=delta_m * xht_eff
    #innalzamento innesti (#inn_eff=ƒ(mon_m , wgt , exp)
    inc_lft_ex = inn_eff * xht_eff
    inc_rgt_ex = inn_eff * xht_eff
    inc_lft_in = inn_eff * xht_eff
    inc_rgt_in = inn_eff * xht_eff

    tch_m = mon_m * tcv_eff
   
    ### Cuting Shape
    if showCuttingShape == True :
        pnt_1 = (wdtSpn_sx_m/2 - tsv_eff/2) , (inc_rgt_in + ((xht_eff + ovs_eff - tch_m - inc_rgt_in) * sqi_eff))
        pnt_2 = (wdtSpn_sx_m/2+tsv_eff/2) , (inc_rgt_ex)
        points_list = [(pnt_1) , (pnt_2)]
        tools.drawPolyByPoints(glyph, points_list)
    
    #### First Span

    ### UpperLeft
    ## punti di ancoraggio
   
    #ext
    pnt_1 = (dis_m) , (xht_eff+ovs_eff)
    hnd_1 = (dis_m + ((-wdtSpn_sx_m/2 - tcv_eff/2 + rtr_extLft_m) * sqe_eff * opt_sqe_lft_m)) , (xht_eff + ovs_eff)
    hnd_2 = (-wdtSpn_sx_m/2 + tsv_eff/2 - rtr_extLft_m - x_inc_upp_m) , ((xht_eff/2) + ((xht_eff/2 + ovs_eff) * sqe_eff))
    pnt_2 = (-wdtSpn_sx_m/2 + tsv_eff/2 - rtr_extLft_m - x_inc_upp_m) , (xht_eff/2  + y_inc_upp_m)
   
    #int
    pnt_3 = (-wdtSpn_sx_m/2 + tsv_eff/2 - x_inc_upp_m) , (xht_eff/2  + y_inc_upp_m)
    hnd_3 = (-wdtSpn_sx_m/2+tsv_eff/2 - x_inc_upp_m) , ((xht_eff/2) + ((xht_eff/2 + ovs_eff - tch_m) * sqi_eff))
    hnd_4 = ((-wdtSpn_sx_m/2+tcv_eff/2) * sqi_eff * opt_sqi_lft_m) , (xht_eff + ovs_eff - tch_m)
    pnt_4 = 0 , (xht_eff + ovs_eff - tch_m)

    ### cut
    # Punti di ancoraggio
    interUpExt = (-wdtSpn_sx_m/2 - tsv_eff/2) , (xht_eff) 
    interUpInt = (-wdtSpn_sx_m/2 + tsv_eff/2) , 0
    cExt = splitCubic (pnt_1,hnd_1,hnd_2,pnt_2,interUpExt[0],False)
    cInt = splitCubic (pnt_3,hnd_3,hnd_4,pnt_4,interUpInt[0],False)
   
    #ext
    pnt_1 = ((cExt [0])[0])
    pnt_2 = ((cExt [0])[3]) 
    hnd_1 = ((cExt [0])[1])
    hnd_2 = ((cExt [0])[2])
       
    #int
    pnt_3 = ((cInt [1])[0])
    pnt_4 = ((cInt [1])[3])
    hnd_3 = ((cInt [1])[1])  
    hnd_4 = ((cInt [1])[2])

   

    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
    ### StemLeft
    ## punti di ancoraggio
   
    pnt_1 = (-wdtSpn_sx_m/2 + tsv_eff/2 - dstSpr_eff - gapSpr_eff) , (xht_eff)
    pnt_2 = (-wdtSpn_sx_m/2 - tsv_eff/2) , (xht_eff)
    pnt_3 = (-wdtSpn_sx_m/2 - tsv_eff/2) , 0
    pnt_4 = (-wdtSpn_sx_m/2 + tsv_eff/2) , 0
    pnt_5 = (-wdtSpn_sx_m/2 + tsv_eff/2) , (xht_eff - innSpr_eff)
    pnt_6 = (-wdtSpn_sx_m/2 + tsv_eff/2 - dstSpr_eff) , (xht_eff - innSpr_eff)
                                         
    points_list = [(pnt_1) , (pnt_2) , (pnt_3) , (pnt_4) ,  (pnt_5) , (pnt_6)]

    #disegno
    tools.drawPolyByPoints(glyph, points_list)

    ### UpperRight
    ## punti di ancoraggio
   
    #ext
    pnt_1 = (wdtSpn_sx_m/2+tsv_eff/2) , (inc_rgt_ex)
    hnd_1 = wdtSpn_sx_m/2+tsv_eff/2 , (inc_rgt_ex + ((xht_eff + ovs_eff - inc_rgt_ex) * sqe_eff))
    hnd_2 = (dis_m + ((wdtSpn_sx_m/2 + tcv_eff/2 - rtr_extLft_m) * sqe_eff * opt_sqe_rgt_m)) , (xht_eff + ovs_eff)
    pnt_2 = (dis_m) , (xht_eff + ovs_eff)
   
    #int
    pnt_3 =  0 , (xht_eff + ovs_eff - tch_m)
    hnd_3 = ((wdtSpn_sx_m/2 - tcv_eff/2) * sqi_eff * opt_sqi_rgt_m) , (xht_eff + ovs_eff - tch_m)
    hnd_4 = (wdtSpn_sx_m/2 - tsv_eff/2) , (inc_rgt_in + ((xht_eff + ovs_eff - tch_m - inc_rgt_in) * sqi_eff))
    pnt_4 = (wdtSpn_sx_m/2-tsv_eff/2) , (inc_rgt_in)
   
    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
                                         
    ### StemRight
    ## punti di ancoraggio
    pnt_1 = (wdtSpn_sx_m/2 + tsv_eff/2) , (inc_rgt_ex)
    pnt_2 = (wdtSpn_sx_m/2 - tsv_eff/2) , (inc_rgt_in)
    pnt_3 = (wdtSpn_sx_m/2 - tsv_eff/2) , 0
    pnt_4 = (wdtSpn_sx_m/2 + tsv_eff/2) , 0
                                     
    points_list = [(pnt_1) , (pnt_2) , (pnt_3) , (pnt_4)]

    #disegno
    tools.drawPolyByPoints(glyph, points_list)
   

    #### SECOND SPAN
   
    ### UpperLeft
    ## punti di ancoraggio

    #ext
    pnt_1 = (dis_m) + wdtSpn_sx_m , (xht_eff+ovs_eff)
    hnd_1 = (dis_m + ((-wdtSpn_dx_m/2 - tcv_eff/2 + rtr_extLft_m) * sqe_eff * opt_sqe_lft_m)) + wdtSpn_sx_m , (xht_eff + ovs_eff)
    hnd_2 = (-wdtSpn_dx_m/2 + tsv_eff/2 - rtr_extLft_m - x_inc_upp_m) + wdtSpn_sx_m , ((xht_eff/2) + ((xht_eff/2 + ovs_eff) * sqe_eff))
    pnt_2 = (-wdtSpn_dx_m/2 + tsv_eff/2 - rtr_extLft_m - x_inc_upp_m) + wdtSpn_sx_m , (xht_eff/2  + y_inc_upp_m)

    #int
    pnt_3 = (-wdtSpn_dx_m/2 + tsv_eff/2 - x_inc_upp_m) + wdtSpn_sx_m , (xht_eff/2  + y_inc_upp_m)
    hnd_3 = (-wdtSpn_dx_m/2+tsv_eff/2 - x_inc_upp_m) + wdtSpn_sx_m , ((xht_eff/2) + ((xht_eff/2 + ovs_eff - tch_m) * sqi_eff))
    hnd_4 = ((-wdtSpn_dx_m/2+tcv_eff/2) * sqi_eff * opt_sqi_lft_m) + wdtSpn_sx_m , (xht_eff + ovs_eff - tch_m)
    pnt_4 = 0 + wdtSpn_sx_m , (xht_eff + ovs_eff - tch_m)

    ### cut
    # Punti di ancoraggio
    if cut == True:

        cutUp1 = ((wdtSpn_sx_m/2 - tcv_eff/2) * sqi_eff * opt_sqi_rgt_m)
        cutUp2 = (wdtSpn_sx_m/2 - tsv_eff/2) , (inc_rgt_in + ((xht_eff + ovs_eff - tch_m - inc_rgt_in) * sqi_eff))
       
        interUpExt = bezTool.calc_int_bez ( [cutUp1,cutUp2,cutUp1,cutUp2] , [pnt_1,hnd_1,hnd_2,pnt_2] )
        interUpInt = bezTool.calc_int_bez ( [cutUp1,cutUp2,cutUp1,cutUp2] , [pnt_3,hnd_3,hnd_4,pnt_4] )
        print 'interUpExt' , interUpExt        
        print 'interUpInt' , interUpInt
        #interUpExt = (dis_m) , (xht_eff) 
        #interUpInt = (-wdtSpn_dx_m/2 + tsv_eff/2)+wdtSpn_dx_m , 0
       
        cExt = splitCubic (pnt_1,hnd_1,hnd_2,pnt_2,interUpExt[0],False)
        cInt = splitCubic (pnt_3,hnd_3,hnd_4,pnt_4,interUpInt[0],False)
       
        #ext
        pnt_1 = ((cExt [0])[0])
        pnt_2 = ((cExt [0])[3]) 
        hnd_1 = ((cExt [0])[1])
        hnd_2 = ((cExt [0])[2])
       
        #int
        pnt_3 = ((cInt [1])[0])
        pnt_4 = ((cInt [1])[3])
        hnd_3 = ((cInt [1])[1])  
        hnd_4 = ((cInt [1])[2])

    else:
        pass


    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
   
    ### UpperRight
    ## punti di ancoraggio

    #ext
    pnt_1 = (wdtSpn_dx_m/2+tsv_eff/2) + wdtSpn_sx_m , (inc_rgt_ex)
    hnd_1 = (wdtSpn_dx_m/2+tsv_eff/2) + wdtSpn_sx_m , (inc_rgt_ex + ((xht_eff + ovs_eff - inc_rgt_ex) * sqe_eff))
    hnd_2 = (dis_m + ((wdtSpn_dx_m/2 + tcv_eff/2 - rtr_extLft_m) * sqe_eff * opt_sqe_rgt_m)) + wdtSpn_sx_m , (xht_eff + ovs_eff)
    pnt_2 = (dis_m) + wdtSpn_sx_m , (xht_eff + ovs_eff)
    #int
    pnt_3 =  0 + wdtSpn_sx_m , (xht_eff + ovs_eff - tch_m)
    hnd_3 = ((wdtSpn_dx_m/2 - tcv_eff/2) * sqi_eff * opt_sqi_rgt_m) + wdtSpn_sx_m , (xht_eff + ovs_eff - tch_m)
    hnd_4 = (wdtSpn_dx_m/2 - tsv_eff/2) + wdtSpn_sx_m , (inc_rgt_in + ((xht_eff + ovs_eff - tch_m - inc_rgt_in) * sqi_eff))
    pnt_4 = (wdtSpn_dx_m/2-tsv_eff/2) + wdtSpn_sx_m , (inc_rgt_in)

    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
                                     
    ### StemRight
    ## punti di ancoraggio
    pnt_1 = (wdtSpn_dx_m/2 + tsv_eff/2) + wdtSpn_sx_m , (inc_rgt_ex)
    pnt_2 = (wdtSpn_dx_m/2 - tsv_eff/2) + wdtSpn_sx_m , (inc_rgt_in)
    pnt_3 = (wdtSpn_dx_m/2 - tsv_eff/2) + wdtSpn_sx_m , 0
    pnt_4 = (wdtSpn_dx_m/2 + tsv_eff/2) + wdtSpn_sx_m , 0
                                 
    points_list = [(pnt_1) , (pnt_2) , (pnt_3) , (pnt_4)]

    #disegno
    tools.drawPolyByPoints(glyph, points_list)
   
    glyph.leftMargin = mrgStm_eff 
    glyph.rightMargin = mrgCrv_eff

    return glyph
   
   
   
# Glyph: h (unicode: 0068)
def drawing_h():
    
    # Calling glyph
    font = CurrentFont ()
    glyph = font['h']
    glyph.clear()
    
    
    ## Variabili Locali [n]
    # Innesti
    x_inc_upp_h = (raccordoUpp_eff)
    y_inc_upp_h = -(raccordoUpp_eff * 2 / inn_eff)
    # Espansione
    exp_h = exp_eff * nor_eff
    # Larghezza
    wdt_h = exp_h * xht_eff
    # Monilineatià (per neutralizzare la monolinearità a valori alti)
    mon_h=tools.lin(mon_eff,1,1,0.01,0.2)
    # Monilineatià 2 (relazione fra punto medio esterno sinistro e monolinearità
    mon_h2=tools.lin(mon_h,1,mon_h/2,0.01,mon_h)
    # Correzioni otticch sqe_lft/sqe_rgt/sqi_lft/sqi_rgt
    opt_sqe_lft_h=1#.95
    opt_sqe_rgt_h=1#tools.lin(sqe_eff,1,1,0.54,1.5)
    opt_sqi_lft_h=1
    opt_sqi_rgt_h=1#tools.lin(sqi_eff,1,1,0.54,1)
    # Rientro punto medio esterno sinistro
    rtr_extLft_h=mon_h2 * tsv_eff
    #disallineamento punto centale superiore ƒ(wgt,mon,exp)
    delta_dis_h_wgt=tools.lin(wgt_nom,0.01,0,0.45,0.04)
    delta_dis_h_mon=tools.lin(mon_nom,0.01,0,1,0.02)
    delta_h=delta_dis_h_wgt+delta_dis_h_mon
    dis_h=delta_h * xht_eff
    #innalzamento innesti (#inn_eff=ƒ(mon_h , wgt , exp)
    inc_lft_ex = inn_eff * xht_eff
    inc_rgt_ex = inn_eff * xht_eff
    inc_lft_in = inn_eff * xht_eff
    inc_rgt_in = inn_eff * xht_eff

    tch_h = mon_h * tcv_eff


    ### UpperLeft
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (dis_h) , (xht_eff+ovs_eff)
    hnd_1 = (dis_h + ((-wdt_h/2 - tcv_eff/2 + rtr_extLft_h) * sqe_eff * opt_sqe_lft_h)) , (xht_eff + ovs_eff)
    hnd_2 = (-wdt_h/2 + tsv_eff/2 - rtr_extLft_h - x_inc_upp_h) , ((xht_eff/2) + ((xht_eff/2 + ovs_eff) * sqe_eff))
    pnt_2 = (-wdt_h/2 + tsv_eff/2 - rtr_extLft_h - x_inc_upp_h) , (xht_eff/2  + y_inc_upp_h)
    #int
    pnt_3 = (-wdt_h/2 + tsv_eff/2 - x_inc_upp_h) , (xht_eff/2  + y_inc_upp_h)
    hnd_3 = (-wdt_h/2+tsv_eff/2 - x_inc_upp_h) , ((xht_eff/2) + ((xht_eff/2 + ovs_eff - tch_h) * sqi_eff))
    hnd_4 = ((-wdt_h/2+tcv_eff/2) * sqi_eff * opt_sqi_lft_h) , (xht_eff + ovs_eff - tch_h)
    pnt_4 = 0 , (xht_eff + ovs_eff - tch_h)

    ### Cut
    # Punti di ancoraggio
    interUpExt = (-wdt_h/2 + tsv_eff/2) , (xht_eff) 
    interUpInt = (-wdt_h/2 + tsv_eff/2) , 0
    cExt = splitCubic (pnt_1,hnd_1,hnd_2,pnt_2,interUpExt[0],False)
    cInt = splitCubic (pnt_3,hnd_3,hnd_4,pnt_4,interUpInt[0],False)
    
    #ext
    pnt_1 = ((cExt [0])[0])
    pnt_2 = ((cExt [0])[3]) 
    hnd_1 = ((cExt [0])[1])
    hnd_2 = ((cExt [0])[2])
    #int
    pnt_3 = ((cInt [1])[0])
    pnt_4 = ((cInt [1])[3])
    hnd_3 = ((cInt [1])[1])  
    hnd_4 = ((cInt [1])[2])
    

    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
    

    ### StemLeft
    ## punti di ancoraggio
    
    pnt_1 = (-wdt_h/2 + tsv_eff/2) , (xht_eff + asc_eff)
    pnt_2 = (-wdt_h/2 - tsv_eff/2) , (xht_eff + asc_eff)
    pnt_3 = (-wdt_h/2 - tsv_eff/2) , 0
    pnt_4 = (-wdt_h/2 + tsv_eff/2) , 0
                                          
    points_list = [(pnt_1) , (pnt_2) , (pnt_3) , (pnt_4)]

    #disegno
    tools.drawPolyByPoints(glyph, points_list)
 

    ### UpperRight
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (wdt_h/2+tsv_eff/2) , (inc_rgt_ex)
    hnd_1 = wdt_h/2+tsv_eff/2 , (inc_rgt_ex + ((xht_eff + ovs_eff - inc_rgt_ex) * sqe_eff))
    hnd_2 = (dis_h + ((wdt_h/2 + tcv_eff/2 - rtr_extLft_h) * sqe_eff * opt_sqe_rgt_h)) , (xht_eff + ovs_eff)
    pnt_2 = (dis_h) , (xht_eff + ovs_eff)
    #int
    pnt_3 =  0 , (xht_eff + ovs_eff - tch_h)
    hnd_3 = ((wdt_h/2 - tcv_eff/2) * sqi_eff * opt_sqi_rgt_h) , (xht_eff + ovs_eff - tch_h)
    hnd_4 = (wdt_h/2 - tsv_eff/2) , (inc_rgt_in + ((xht_eff + ovs_eff - tch_h - inc_rgt_in) * sqi_eff))
    pnt_4 = (wdt_h/2-tsv_eff/2) , (inc_rgt_in)
    
    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
                                          
    ### StemRight
    ## punti di ancoraggio
    
    pnt_1 = (wdt_h/2 + tsv_eff/2) , (inc_rgt_ex)
    pnt_2 = (wdt_h/2 - tsv_eff/2) , (inc_rgt_in)
    pnt_3 = (wdt_h/2 - tsv_eff/2) , 0
    pnt_4 = (wdt_h/2 + tsv_eff/2) , 0
                                      
    points_list = [(pnt_1) , (pnt_2) , (pnt_3) , (pnt_4)]

    #disegno
    tools.drawPolyByPoints(glyph, points_list)
    
    glyph.leftMargin = mrgStm_eff 
    glyph.rightMargin = mrgCrv_eff
    
    return glyph



# Glyph: u (unicode: 0075)
def drawing_u(glyphToAppend):
    
    # Calling glyph
    font = CurrentFont ()
    glyph = font['u']
    glyph.clear()
    
    glyph.appendGlyph(glyphToAppend, (0,0))
    
    glyph.rotate ((180))
    glyph.move ((0,500))
    
    glyph.leftMargin = mrgCrv_eff 
    glyph.rightMargin = mrgStm_eff
    
    return glyph
    


# Glyph: p (unicode: 0070)
def drawing_p():
    
    # Calling glyph
    font = CurrentFont ()
    glyph = font['p']
    glyph.clear()
    
    
    ## Variabili locali [p]
    # Larghezza
    wdtRgt_p = (2 * exp_eff / (nor_eff + 1)) * xht_eff
    wdtLft_p = (exp_eff * nor_eff) * xht_eff
    # Innesti
    x_inc_upp_p = (raccordoUpp_eff)
    y_inc_upp_p = -(raccordoUpp_eff * 2 / inn_eff)

    x_inc_dwn_p = (raccordoDwn_eff)    
    y_inc_dwn_p = -(raccordoDwn_eff * 2 / inn_eff)
    # Squadrature specifiche sqe/sqi
    delta_sqe_nor_p = tools.lin(nor_nom,1,0,0.75,0.3*(1-gsq_nom))
    sqeRgt_p = sqe_eff + delta_sqe_nor_p
    sqeLft_p = sqe_eff - delta_sqe_nor_p
    
    delta_sqi_nor_p = tools.lin(nor_nom,1,0,0.75,0.3*(1-gsq_nom))
    sqiRgt_p = sqi_eff + delta_sqi_nor_p
    sqiLft_p = sqi_eff - delta_sqi_nor_p
    # correzioni ottiche sqe
    optSqe_upperLeft =1# tools.lin(sqe_eff,1,1,0.54,1.3)
    optSqe_bottomLeft =1# tools.lin(sqe_eff,1,1,0.54,1.35)
    optSqe_upperRight =1# tools.lin(sqe_eff,1,1,0.54,1)
    optSqe_bottomRight =1# optSqe_upperRight
    # correzioni ottiche sqi
    optSqi_upperLeft =1# tools.lin(sqi_eff,1,1,0.54,2.18)
    optSqi_bottomLeft =1# optSqe_upperLeft
    optSqi_upperRight =1# tools.lin(sqi_eff,1,1,0.54,1)
    optSqi_bottomRight =1# tools.lin(sqi_eff,1,1,0.54,1.225)
    # Monilineatià (per neutralizzare la monolinearità a valori alti)
    mon_p = tools.lin(mon_eff,1,1,0.01,0.2)
    # Monilineatià 2 (relazione fra punto medio esterno sinistro e monolinearità
    mon_p2 = tools.lin(mon_p,1,mon_p/2,0.01,mon_p)
    # Disallineamento orizzontale p ƒ(wgt,mon,exp)
    delta_dis_p_wgt = tools.lin(wgt_eff,0.1,0,0.45,0.04)
    delta_dis_p_mon = tools.lin(mon_eff,0.01,0,1,0.02)
    dis_p = (delta_dis_p_wgt + delta_dis_p_mon) * xht_eff
    # Spessore curve orizzontali
    tch_p = tcv_eff * mon_p
    # Rientro punto medio esterno sinistro
    rtr_extLft_p = mon_p2 * tsv_eff
    
    
    ### UpperLeft
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (dis_p) , (xht_eff + ovs_eff)
    hnd_1 = ((-wdtLft_p/2-tcv_eff/2+ rtr_extLft_p+dis_p)*sqeLft_p * optSqe_upperLeft) , (xht_eff + ovs_eff)
    hnd_2 = (-wdtLft_p/2 + tcv_eff/2 - rtr_extLft_p - x_inc_upp_p) , ((xht_eff/2) + ((xht_eff/2+ovs_eff)*sqeLft_p))
    pnt_2 = (-wdtLft_p/2 + tcv_eff/2 - rtr_extLft_p - x_inc_upp_p) , (xht_eff/2  + y_inc_upp_p)
    #int
    pnt_3 = (-wdtLft_p/2 + tcv_eff/2 - x_inc_upp_p) , (xht_eff/2 + y_inc_upp_p)
    hnd_3 = (-wdtLft_p/2 + tcv_eff/2 - x_inc_upp_p) , ((xht_eff/2) + (xht_eff/2 - ovs_eff - tch_p) * sqiLft_p)
    hnd_4 = ((-wdtLft_p/2 + tcv_eff/2 - rtr_extLft_p - dis_p) * sqiLft_p * optSqi_upperLeft) , (xht_eff + ovs_eff - tch_p)
    pnt_4 = 0 , (xht_eff + ovs_eff - tch_p)

    ### cut
    # Punti di ancoraggio
    interUpExt = (-wdtLft_p/2 - tsv_eff/2) , xht_eff 
    interUpInt = (-wdtLft_p/2 + tsv_eff/2) , -dsc_eff
    cExt = splitCubic (pnt_1,hnd_1,hnd_2,pnt_2,interUpExt[0],False)
    cInt = splitCubic (pnt_3,hnd_3,hnd_4,pnt_4,interUpInt[0],False)
    
    #ext
    pnt_1 = ((cExt [0])[0])
    pnt_2 = ((cExt [0])[3]) 
    hnd_1 = ((cExt [0])[1])
    hnd_2 = ((cExt [0])[2])
    #int
    pnt_3 = ((cInt [1])[0])
    pnt_4 = ((cInt [1])[3])
    hnd_3 = ((cInt [1])[1])  
    hnd_4 = ((cInt [1])[2])
    
    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
 
    ### StemLeft
    ## punti di ancoraggio
    
    pnt_1 = (-wdtLft_p/2 + tsv_eff/2 - dstSpr_eff - gapSpr_eff) , xht_eff
    pnt_2 = (-wdtLft_p/2 - tsv_eff/2) , xht_eff
    pnt_3 = (-wdtLft_p/2 - tsv_eff/2) , -dsc_eff                                      
    pnt_4 = (-wdtLft_p/2 + tsv_eff/2) , -dsc_eff
    pnt_5 = (-wdtLft_p/2 + tsv_eff/2) , (xht_eff - innSpr_eff)
    pnt_6 = (-wdtLft_p/2 + tsv_eff/2 - dstSpr_eff) , (xht_eff - innSpr_eff)

    points_list = [(pnt_1) , (pnt_2) , (pnt_3) , (pnt_4) , (pnt_5) , (pnt_6)]

    #disegno
    tools.drawPolyByPoints(glyph, points_list)
    

    ### BottomLeft
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (-wdtLft_p/2 + tcv_eff/2 - rtr_extLft_p - x_inc_dwn_p) , (xht_eff/2  - y_inc_dwn_p)
    hnd_1 = (-wdtLft_p/2 + tcv_eff/2 - rtr_extLft_p - x_inc_dwn_p) , ((xht_eff/2) + ((-xht_eff/2-ovs_eff) * sqeLft_p))
    hnd_2 = ((-wdtLft_p/2 - tcv_eff/2 + rtr_extLft_p + dis_p) * sqeLft_p * optSqe_bottomLeft) , (-ovs_eff) 
    pnt_2 = (dis_p) , (-ovs_eff)
    #int
    pnt_3 = 0 , (tch_p - ovs_eff)
    hnd_3 = ((-wdtLft_p/2 + tcv_eff/2 - rtr_extLft_p - dis_p) * sqiLft_p * optSqi_bottomLeft) , (tch_p - ovs_eff)
    hnd_4 = (-wdtLft_p/2 + tcv_eff/2 - x_inc_dwn_p) , ((xht_eff/2) + ((-xht_eff/2 + ovs_eff + tch_p) * sqiLft_p))
    pnt_4 = (-wdtLft_p/2 + tcv_eff/2 - x_inc_dwn_p) , (xht_eff/2 - y_inc_dwn_p)

    ### cut
    # Punti di ancoraggio
    interUpExt = (-wdtLft_p/2 + tsv_eff/2) , xht_eff 
    interUpInt = (-wdtLft_p/2 + tsv_eff/2) , -dsc_eff
    cExt = splitCubic (pnt_1,hnd_1,hnd_2,pnt_2,interUpExt[0],False)
    cInt = splitCubic (pnt_3,hnd_3,hnd_4,pnt_4,interUpInt[0],False)
    
    #ext
    pnt_1 = ((cExt [1])[0])
    pnt_2 = ((cExt [1])[3]) 
    hnd_1 = ((cExt [1])[1])
    hnd_2 = ((cExt [1])[2])
    #int
    pnt_3 = ((cInt [0])[0])
    pnt_4 = ((cInt [0])[3])
    hnd_3 = ((cInt [0])[1])  
    hnd_4 = ((cInt [0])[2])
    
    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
      

    ### BottomRight
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (dis_p) , (-ovs_eff)
    hnd_1 = ((wdtRgt_p/2 + tcv_eff/2 - dis_p) * sqeRgt_p * optSqe_bottomRight) , (-ovs_eff)
    hnd_2 = (wdtRgt_p/2 + tcv_eff/2) , ((xht_eff/2) + (-xht_eff/2 - ovs_eff) *sqeRgt_p)
    pnt_2 = (wdtRgt_p/2 + tcv_eff/2) , (xht_eff/2)
    #int
    pnt_3 = (wdtRgt_p/2 - tcv_eff/2) , (xht_eff/2)
    hnd_3 = (wdtRgt_p/2 - tcv_eff/2) , ((xht_eff/2) + (-xht_eff/2 + ovs_eff + tch_p) *sqiRgt_p)
    hnd_4 = ((wdtRgt_p/2 - tcv_eff/2 - dis_p) * sqiRgt_p * optSqi_bottomRight) , (tch_p - ovs_eff)
    pnt_4 = 0 , (tch_p - ovs_eff)

    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
    

    ### UpperRight
    ## punti di ancoraggio
    
    #ext
    pnt_1 = (wdtRgt_p/2 + tcv_eff/2) , (xht_eff/2)
    hnd_1 = (wdtRgt_p/2 + tcv_eff/2) , ((xht_eff/2) + (xht_eff/2 - ovs_eff) * sqeRgt_p )
    hnd_2 = ((wdtRgt_p/2 + tcv_eff/2 - dis_p) * sqeRgt_p * optSqe_upperRight) , (xht_eff + ovs_eff) #(dis_p) + 
    pnt_2 = (dis_p) , (xht_eff + ovs_eff)
    #int
    pnt_3 = 0 , (xht_eff + ovs_eff - tch_p)
    hnd_3 = ((wdtRgt_p/2 - tcv_eff/2 - dis_p) * sqiRgt_p * optSqi_upperRight) , (xht_eff + ovs_eff - tch_p)
    hnd_4 = (wdtRgt_p/2 - tcv_eff/2) , ((xht_eff/2) + (xht_eff/2 - ovs_eff - tch_p) * sqiRgt_p)
    pnt_4 = (wdtRgt_p/2 - tcv_eff/2) , (xht_eff/2)

    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))

    glyph.leftMargin = mrgStm_eff 
    glyph.rightMargin = mrgCrv_eff

    return glyph



# Glyph: q (unicode: 0071)
def drawing_q():
    
    # Calling glyph
    font = CurrentFont ()
    glyph = font['q']
    glyph.clear()
    
    
    ## Variabili locali [p]
    # Larghezza
    wdtLft_q = (2 * exp_eff / (nor_eff + 1)) * xht_eff
    wdtRgt_q = (exp_eff * nor_eff) * xht_eff
    # Innesti
    x_inc_upp_q = (raccordoUpp_eff)
    y_inc_upp_q = -(raccordoUpp_eff * 2 / inn_eff)

    x_inc_dwn_q = (raccordoDwn_eff)    
    y_inc_dwn_q = -(raccordoDwn_eff * 2 / inn_eff)
    # Squadrature specifiche sqe/sqi
    delta_sqe_nor_q = tools.lin(nor_nom,1,0,0.75,0.3*(1-gsq_nom))
    sqeRgt_q = sqe_eff + delta_sqe_nor_q
    sqeLft_q = sqe_eff - delta_sqe_nor_q
    
    delta_sqi_nor_q = tools.lin(nor_nom,1,0,0.75,0.3*(1-gsq_nom))
    sqiRgt_q = sqi_eff + delta_sqi_nor_q
    sqiLft_q = sqi_eff - delta_sqi_nor_q
    # correzioni ottiche sqe
    optSqe_upperLeft = 1# tools.lin(sqe_eff,1,1,0.54,1.3)
    optSqe_bottomLeft = 1# tools.lin(sqe_eff,1,1,0.54,1.35)
    optSqe_upperRight = 1# tools.lin(sqe_eff,1,1,0.54,1)
    optSqe_bottomRight = 1# optSqe_upperRight
    # correzioni ottiche sqi
    optSqi_upperLeft = 1# tools.lin(sqi_eff,1,1,0.54,2.18)
    optSqi_bottomLeft = 1# optSqe_upperLeft
    optSqi_upperRight = 1# tools.lin(sqi_eff,1,1,0.54,1)
    optSqi_bottomRight = 1# tools.lin(sqi_eff,1,1,0.54,1.225)
    # Monilineatià (per neutralizzare la monolinearità a valori alti)
    mon_q = tools.lin(mon_eff,1,1,0.01,0.2)
    # Monilineatià 2 (relazione fra punto medio esterno sinistro e monolinearità
    mon_q2 = tools.lin(mon_q,1,mon_q/2,0.01,mon_q)
    # Disallineamento orizzontale p ƒ(wgt,mon,exp)
    delta_dis_q_wgt = tools.lin(wgt_eff,0.1,0,0.45,0.04)
    delta_dis_q_mon = tools.lin(mon_eff,0.01,0,1,0.02)
    dis_q = (delta_dis_q_wgt + delta_dis_q_mon) * xht_eff
    # Spessore curve orizzontali
    tch_q = tcv_eff * mon_q
    # Rientro punto medio esterno sinistro
    rtr_extLft_q = mon_q2 * tsv_eff
    
    
    ### UpperLeft
    ## punti di ancoraggio
    
    #ext
    pnt_1 = -(dis_q) , (xht_eff + ovs_eff)
    hnd_1 = -((-wdtLft_q/2 - tcv_eff/2 + rtr_extLft_q + dis_q)*sqeLft_q * optSqe_upperLeft) , (xht_eff + ovs_eff)
    hnd_2 = -(-wdtLft_q/2 + tcv_eff/2 - rtr_extLft_q - x_inc_upp_q) , ((xht_eff/2) + ((xht_eff/2+ovs_eff)*sqeLft_q))
    pnt_2 = -(-wdtLft_q/2 + tcv_eff/2 - rtr_extLft_q - x_inc_upp_q) , (xht_eff/2  + y_inc_upp_q)
    #int
    pnt_3 = -(-wdtLft_q/2 + tcv_eff/2 - x_inc_upp_q) , (xht_eff/2 + y_inc_upp_q)
    hnd_3 = -(-wdtLft_q/2 + tcv_eff/2 - x_inc_upp_q) , ((xht_eff/2) + (xht_eff/2 - ovs_eff - tch_q) * sqiLft_q)
    hnd_4 = -((-wdtLft_q/2 + tcv_eff/2) * sqiLft_q * optSqi_upperLeft) , (xht_eff + ovs_eff - tch_q)
    pnt_4 = 0 , (xht_eff + ovs_eff - tch_q)
    
    ### Cut
    # Punti di ancoraggio
    interUpExt = -(-wdtLft_q/2 - tsv_eff/2) , xht_eff 
    interUpInt = -(-wdtLft_q/2 + tsv_eff/2) , -dsc_eff
    cExt = splitCubic (pnt_1,hnd_1,hnd_2,pnt_2,interUpExt[0],False)
    cInt = splitCubic (pnt_3,hnd_3,hnd_4,pnt_4,interUpInt[0],False)
    
    #ext
    pnt_1 = ((cExt [0])[0])
    pnt_2 = ((cExt [0])[3]) 
    hnd_1 = ((cExt [0])[1])
    hnd_2 = ((cExt [0])[2])
    #int
    pnt_3 = ((cInt [1])[0])
    pnt_4 = ((cInt [1])[3])
    hnd_3 = ((cInt [1])[1])  
    hnd_4 = ((cInt [1])[2])
    
    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
    

    ### StemLeft
    ## punti di ancoraggio
    
    pnt_1 = -(-wdtLft_q/2 + tsv_eff/2 - dstSpr_eff - gapSpr_eff) , xht_eff
    pnt_2 = -(-wdtLft_q/2 - tsv_eff/2) , xht_eff
    pnt_3 = -(-wdtLft_q/2 - tsv_eff/2) , -dsc_eff                                      
    pnt_4 = -(-wdtLft_q/2 + tsv_eff/2) , -dsc_eff
    pnt_5 = -(-wdtLft_q/2 + tsv_eff/2) , (xht_eff - innSpr_eff)
    pnt_6 = -(-wdtLft_q/2 + tsv_eff/2 - dstSpr_eff) , (xht_eff - innSpr_eff)

    points_list = [(pnt_1) , (pnt_2) , (pnt_3) , (pnt_4) , (pnt_5) , (pnt_6)]

    #disegno
    tools.drawPolyByPoints(glyph, points_list)


    ### BottomLeft
    ## punti di ancoraggio
    
    #ext
    pnt_1 = -(-wdtLft_q/2 + tcv_eff/2 - rtr_extLft_q - x_inc_dwn_q) , (xht_eff/2  - y_inc_dwn_q)
    hnd_1 = -(-wdtLft_q/2 + tcv_eff/2 - rtr_extLft_q - x_inc_dwn_q) , ((xht_eff/2) + ((-xht_eff/2-ovs_eff) * sqeLft_q))
    hnd_2 = -((-wdtLft_q/2 - tcv_eff/2 + rtr_extLft_q + dis_q) * sqeLft_q * optSqe_bottomLeft) , (-ovs_eff) #(dis_q) + 
    pnt_2 = -(dis_q) , (-ovs_eff)
    
    #int
    pnt_3 = 0 , (tch_q - ovs_eff)
    hnd_3 = -((-wdtLft_q/2 + tcv_eff/2) * sqiLft_q * optSqi_bottomLeft) , (tch_q - ovs_eff)
    hnd_4 = -(-wdtLft_q/2 + tcv_eff/2 - x_inc_dwn_q) , ((xht_eff/2) + ((-xht_eff/2 + ovs_eff + tch_q) * sqiLft_q))
    pnt_4 = -(-wdtLft_q/2 + tcv_eff/2 - x_inc_dwn_q) , (xht_eff/2 - y_inc_dwn_q)

    ### cut
    # Punti di ancoraggio
    interUpExt = -(-wdtLft_q/2 + tsv_eff/2) , xht_eff 
    interUpInt = -(-wdtLft_q/2 + tsv_eff/2) , -dsc_eff
    cExt = splitCubic (pnt_1,hnd_1,hnd_2,pnt_2,interUpExt[0],False)
    cInt = splitCubic (pnt_3,hnd_3,hnd_4,pnt_4,interUpInt[0],False)
    
    #ext
    pnt_1 = ((cExt [1])[0])
    pnt_2 = ((cExt [1])[3]) 
    hnd_1 = ((cExt [1])[1])
    hnd_2 = ((cExt [1])[2])
        
    #int
    pnt_3 = ((cInt [0])[0])
    pnt_4 = ((cInt [0])[3])
    hnd_3 = ((cInt [0])[1])  
    hnd_4 = ((cInt [0])[2])
    
    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
              
    ### BottomRight
    ## punti di ancoraggio
    
    #ext
    pnt_1 = -(dis_q) , (-ovs_eff)
    hnd_1 = -((wdtRgt_q/2 + tcv_eff/2 - dis_q) * sqeRgt_q * optSqe_bottomRight) , (-ovs_eff)
    hnd_2 = -(wdtRgt_q/2 + tcv_eff/2) , ((xht_eff/2) + (-xht_eff/2 - ovs_eff) *sqeRgt_q)
    pnt_2 = -(wdtRgt_q/2 + tcv_eff/2) , (xht_eff/2)
    
    #int
    pnt_3 = -(wdtRgt_q/2 - tcv_eff/2) , (xht_eff/2)
    hnd_3 = -(wdtRgt_q/2 - tcv_eff/2) , ((xht_eff/2) + (-xht_eff/2 + ovs_eff + tch_q) *sqiRgt_q)
    hnd_4 = -((wdtRgt_q/2 - tcv_eff/2 - dis_q) * sqiRgt_q * optSqi_bottomRight) , (tch_q - ovs_eff)
    pnt_4 = 0 , (tch_q - ovs_eff)

    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))
              
    ### UpperRight
    ## punti di ancoraggio
    
    #ext
    pnt_1 = -(wdtRgt_q/2 + tcv_eff/2) , (xht_eff/2)
    hnd_1 = -(wdtRgt_q/2 + tcv_eff/2) , ((xht_eff/2) + (xht_eff/2 - ovs_eff) * sqeRgt_q )
    hnd_2 = -((wdtRgt_q/2 + tcv_eff/2 - dis_q) * sqeRgt_q * optSqe_upperRight) , (xht_eff + ovs_eff) #(dis_q) + 
    pnt_2 = -(dis_q) , (xht_eff + ovs_eff)
    
    #int
    pnt_3 = 0 , (xht_eff + ovs_eff - tch_q)
    hnd_3 = -((wdtRgt_q/2 - tcv_eff/2 - dis_q) * sqiRgt_q * optSqi_upperRight) , (xht_eff + ovs_eff - tch_q)
    hnd_4 = -(wdtRgt_q/2 - tcv_eff/2) , ((xht_eff/2) + (xht_eff/2 - ovs_eff - tch_q) * sqiRgt_q)
    pnt_4 = -(wdtRgt_q/2 - tcv_eff/2) , (xht_eff/2)

    # Disegno
    tools.genericQuarter(glyph,
                  (pnt_1),
                  (hnd_1), (hnd_2), (pnt_2),
                  (pnt_3),
                  (hnd_3), (hnd_4), (pnt_4))

    glyph.leftMargin = mrgCrv_eff 
    glyph.rightMargin = mrgStm_eff
    return glyph



# Glyph: b (unicode: 0062)
def drawing_b(glyphToAppend):
    
    # Calling glyph
    font = CurrentFont ()
    glyph = font['b']
    glyph.clear()
    
    glyph.appendGlyph(glyphToAppend, (0,0))
    
    glyph.rotate ((180))
    glyph.move ((0,500))
    
    glyph.leftMargin = mrgStm_eff 
    glyph.rightMargin = mrgCrv_eff

    return glyph



# Glyph: d (unicode: 0064)
def drawing_d(glyphToAppend):
    
    # Calling glyph
    font = CurrentFont ()
    glyph = font['d']
    glyph.clear()
    
    glyph.appendGlyph(glyphToAppend, (0,0))
    
    glyph.rotate ((180))
    glyph.move ((0,500))
    
    glyph.leftMargin = mrgCrv_eff 
    glyph.rightMargin = mrgStm_eff

    return glyph



# Glyph: l (unicode: 006C)
def drawing_l():

    # Calling glyph
    font = CurrentFont ()
    glyph = font['l']
    glyph.clear()
    
    
    pnt_1 = (tsv_eff/2) , (xht_eff + asc_eff)
    pnt_2 = (-tsv_eff/2) , (xht_eff + asc_eff)
    pnt_3 = (-tsv_eff/2) , 0
    pnt_4 = (tsv_eff/2) , 0
                                          
    points_list = [(pnt_1) , (pnt_2) , (pnt_3) , (pnt_4)]

    #disegno
    tools.drawPolyByPoints(glyph, points_list)
    
    glyph.leftMargin = mrgStm_eff 
    glyph.rightMargin = mrgStm_eff
    
    return glyph



# Glyph: i (unicode: 0069)
def drawing_i():

    # Calling glyph
    font = CurrentFont ()
    glyph = font['i']
    glyph.clear()
    
    
    ## Variabili locali [i]
    #spazio
    delta_spc_i = tools.par (wgt_eff , 0.04 , 0.31 , 0.18 , 0.25 , 0.31 , 0.23)
    spc_i =(xht_eff * delta_spc_i)
    # Spessore punto
    delta_tckDot_i = tools.par (wgt_eff , 0.04 , 0.12 , 0.18 , 0.18 , 0.31 , 0.20)
    tckDot_i =(xht_eff * delta_tckDot_i)

    # Stem
    pnt_1 = (tsv_eff/2) , (xht_eff)
    pnt_2 = (-tsv_eff/2) , (xht_eff)
    pnt_3 = (-tsv_eff/2) , 0
    pnt_4 = (tsv_eff/2) , 0
                                          
    points_list = [(pnt_1) , (pnt_2) , (pnt_3) , (pnt_4)]

    #disegno
    tools.drawPolyByPoints(glyph, points_list)
    
    # dot
    pnt_4 = (tsv_eff/2) , (xht_eff + spc_i )
    pnt_3 = (-tsv_eff/2) , (xht_eff + spc_i )
    pnt_2 = (-tsv_eff/2) , (xht_eff + spc_i + tckDot_i)
    pnt_1 = (tsv_eff/2) , (xht_eff + spc_i + tckDot_i)
                                              
    points_list = [(pnt_1) , (pnt_2) , (pnt_3) , (pnt_4)]

    #disegno
    tools.drawPolyByPoints(glyph, points_list)
    
    glyph.leftMargin = mrgStm_eff 
    glyph.rightMargin = mrgStm_eff

    return glyph



# Glyph draw

c = drawing_c()
#e = drawing_e()
h = drawing_h()
i = drawing_i()
l = drawing_l()
m = drawing_m()
n = drawing_n()
o = drawing_o()
p = drawing_p()
q = drawing_q()
#r = drawing_r()

d = drawing_d(p)
u = drawing_u(n)
b = drawing_b(q)