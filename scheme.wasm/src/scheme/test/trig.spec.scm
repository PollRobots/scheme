#|
 |  Test trig operations.
 |#
(if (not (procedure? assert))
  (include "test/test.scm"))

(run-tests "trig"
  (test-case "sin²x + cos²x = 1" (lambda ()
    (letrec ((id (lambda (x)
                (let ((sx (sin x)) (cx (cos x)))
                  (+ (* sx sx) (* cx cx)))))
             (test (lambda (n)
                (cond
                  ((< n 6.3) ;; test up to 2π
                    (assert
                      (< (abs (- 1 (id n))) 1e-13)
                      " sin²x + cos²x of " n " = " (id n))
                    (test (+ n 0.05)))
                  (else #t)))))
      (test 0))))

  (test-case "tan x = sin x / cos x" (lambda ()
    (letrec ((id (lambda (x)
                  (let ((dx (- (tan x) (/ (sin x) (cos x)))))
                    (* dx dx))))
             (test (lambda (acc x n)
                (cond
                  ((< x 6.3) ;; test up to 2π
                   (assert
                      (< (id x) 1e-20)
                      "(tan x - sin x / cos x)² of " x " = " (id x))
                   (test (+ acc (id x)) (+ x 0.05) (+ n 1)))
                  (else (let ((rms (sqrt (/ acc n))))
                          (assert (< rms 1e-12) " rms error is " rms)))))))

      (test 0 0 0))))

  (test-case "tan x - test vectors over 0 ≤ x < π/4" (lambda ()
    (let ((test-vector #( (0.00	0.000000000000000)
                          (0.01	0.010000333346667)
                          (0.02	0.020002667093402)
                          (0.03	0.030009003241181)
                          (0.04	0.040021346995515)
                          (0.05	0.050041708375539)
                          (0.06	0.060072103831297)
                          (0.07	0.070114557872003)
                          (0.08	0.080171104708073)
                          (0.09	0.090243789909785)
                          (0.10	0.100334672085451)
                          (0.11	0.110445824582041)
                          (0.12	0.120579337211305)
                          (0.13	0.130737318004460)
                          (0.14	0.140921894998625)
                          (0.15	0.151135218058295)
                          (0.16	0.161379460735211)
                          (0.17	0.171656822170143)
                          (0.18	0.181969529040198)
                          (0.19	0.192319837555433)
                          (0.20	0.202710035508673)
                          (0.21	0.213142444382645)
                          (0.22	0.223619421518684)
                          (0.23	0.234143362351465)
                          (0.24	0.244716702714465)
                          (0.25	0.255341921221036)
                          (0.26	0.266021541726265)
                          (0.27	0.276758135875031)
                          (0.28	0.287554325741977)
                          (0.29	0.298412786569432)
                          (0.30	0.309336249609623)
                          (0.31	0.320327505077924)
                          (0.32	0.331389405224235)
                          (0.33	0.342524867530039)
                          (0.34	0.353736878039123)
                          (0.35	0.365028494830425)
                          (0.36	0.376402851642027)
                          (0.37	0.387863161655849)
                          (0.38	0.399412721453226)
                          (0.39	0.411054915152214)
                          (0.40	0.422793218738162)
                          (0.41	0.434631204599889)
                          (0.42	0.446572546284595)
                          (0.43	0.458621023485555)
                          (0.44	0.470780527277622)
                          (0.45	0.483055065616578)
                          (0.46	0.495448769119550)
                          (0.47	0.507965897144883)
                          (0.48	0.520610844191258)
                          (0.49	0.533388146637203)
                          (0.50	0.546302489843790)
                          (0.51	0.559358715644945)
                          (0.52	0.572561830251668)
                          (0.53	0.585917012598471)
                          (0.54	0.599429623162490)
                          (0.55	0.613105213288136)
                          (0.56	0.626949535052698)
                          (0.57	0.640968551711156)
                          (0.58	0.655168448761508)
                          (0.59	0.669555645675302)
                          (0.60	0.684136808341692)
                          (0.61	0.698918862277391)
                          (0.62	0.713909006659240)
                          (0.63	0.729114729240969)
                          (0.64	0.744543822220964)
                          (0.65	0.760204399133676)
                          (0.66	0.776104912843664)
                          (0.67	0.792254174728257)
                          (0.68	0.808661375142565)
                          (0.69	0.825336105269025)
                          (0.70	0.842288380463079)
                          (0.71	0.859528665216941)
                          (0.72	0.877067899874834)
                          (0.73	0.894917529245814)
                          (0.74	0.913089533274301)
                          (0.75	0.931596459944072)
                          (0.76	0.950451460608803)
                          (0.77	0.969668327961489)
                          (0.78	0.989261536876605))))
      (letrec ((fn (lambda (acc i)
                      (if (> i 78) ;; check for range 0 - π/4
                          (sqrt (/ acc i))
                          (let* ((v (vector-ref test-vector i))
                                 (d (- (tan (car v)) (cadr v))))
                            (fn (+ acc (* d d)) (+ i 1)))))))
          (let ((rms-error (fn 0 0)))
            (assert
              (< rms-error 1e-13)
              " expect rms error to be less than 1e-13, got " rms-error))))))

  (test-case "atan x - test vectors over 0 ≤ x ≤ 1" (lambda ()
    (let ((test-vector #( (0.00	0.00000000000000)
                          (0.01	0.00999966668666)
                          (0.02	0.01999733397315)
                          (0.03	0.02999100485687)
                          (0.04	0.03997868712329)
                          (0.05	0.04995839572194)
                          (0.06	0.05992815512120)
                          (0.07	0.06988600163464)
                          (0.08	0.07982998571223)
                          (0.09	0.08975817418995)
                          (0.10	0.09966865249116)
                          (0.11	0.10955952677394)
                          (0.12	0.11942892601833)
                          (0.13	0.12927500404814)
                          (0.14	0.13909594148207)
                          (0.15	0.14888994760949)
                          (0.16	0.15865526218640)
                          (0.17	0.16839015714753)
                          (0.18	0.17809293823119)
                          (0.19	0.18776194651359)
                          (0.20	0.19739555984988)
                          (0.21	0.20699219421982)
                          (0.22	0.21655030497608)
                          (0.23	0.22606838799388)
                          (0.24	0.23554498072086)
                          (0.25	0.24497866312686)
                          (0.26	0.25436805855326)
                          (0.27	0.26371183446226)
                          (0.28	0.27300870308671)
                          (0.29	0.28225742198149)
                          (0.30	0.29145679447786)
                          (0.31	0.30060567004239)
                          (0.32	0.30970294454245)
                          (0.33	0.31874756042064)
                          (0.34	0.32773850678055)
                          (0.35	0.33667481938672)
                          (0.36	0.34555558058171)
                          (0.37	0.35437991912343)
                          (0.38	0.36314700994617)
                          (0.39	0.37185607384858)
                          (0.40	0.38050637711236)
                          (0.41	0.38909723105527)
                          (0.42	0.39762799152212)
                          (0.43	0.40609805831761)
                          (0.44	0.41450687458478)
                          (0.45	0.42285392613294)
                          (0.46	0.43113874071878)
                          (0.47	0.43936088728459)
                          (0.48	0.44751997515717)
                          (0.49	0.45561565321122)
                          (0.50	0.46364760900080)
                          (0.51	0.47161556786232)
                          (0.52	0.47951929199259)
                          (0.53	0.48735857950519)
                          (0.54	0.49513326346840)
                          (0.55	0.50284321092786)
                          (0.56	0.51048832191677)
                          (0.57	0.51806852845672)
                          (0.58	0.52558379355161)
                          (0.59	0.53303411017749)
                          (0.60	0.54041950027058)
                          (0.61	0.54774001371590)
                          (0.62	0.55499572733858)
                          (0.63	0.56218674390002)
                          (0.64	0.56931319110066)
                          (0.65	0.57637522059118)
                          (0.66	0.58337300699385)
                          (0.67	0.59030674693537)
                          (0.68	0.59717665809267)
                          (0.69	0.60398297825299)
                          (0.70	0.61072596438920)
                          (0.71	0.61740589175157)
                          (0.72	0.62402305297675)
                          (0.73	0.63057775721493)
                          (0.74	0.63707032927568)
                          (0.75	0.64350110879328)
                          (0.76	0.64987044941194)
                          (0.77	0.65617871799139)
                          (0.78	0.66242629383315)
                          (0.79	0.66861356792782)
                          (0.80	0.67474094222355)
                          (0.81	0.68080882891582)
                          (0.82	0.68681764975864)
                          (0.83	0.69276783539712)
                          (0.84	0.69865982472146)
                          (0.85	0.70449406424221)
                          (0.86	0.71027100748668)
                          (0.87	0.71599111441630)
                          (0.88	0.72165485086476)
                          (0.89	0.72726268799669)
                          (0.90	0.73281510178650)
                          (0.91	0.73831257251722)
                          (0.92	0.74375558429886)
                          (0.93	0.74914462460601)
                          (0.94	0.75448018383440)
                          (0.95	0.75976275487577)
                          (0.96	0.76499283271091)
                          (0.97	0.77017091402033)
                          (0.98	0.77529749681212)
                          (0.99	0.78037308006663)
                          (1.00	0.78539816339744))))
      (letrec ((fn (lambda (acc i)
                      (if (> i 100)
                          (sqrt (/ acc i))
                          (let* ((v (vector-ref test-vector i))
                                 (d (- (atan (car v)) (cadr v))))
                            (fn (+ acc (* d d)) (+ i 1)))))))
          (let ((rms-error (fn 0 0)))
            (assert
              (< rms-error 1e-14)
              " expect rms error to be less than 1e-14, got " rms-error))))))

  (test-case "sin x - test vectors over 0 ≤ x ≤ π/2" (lambda ()
    (let ((test-vector #( (0.00	0.0000000000000000)
                          (0.01	0.0099998333341667)
                          (0.02	0.0199986666933331)
                          (0.03	0.0299955002024957)
                          (0.04	0.0399893341866342)
                          (0.05	0.0499791692706783)
                          (0.06	0.0599640064794446)
                          (0.07	0.0699428473375328)
                          (0.08	0.0799146939691727)
                          (0.09	0.0898785491980110)
                          (0.10	0.0998334166468282)
                          (0.11	0.1097783008371750)
                          (0.12	0.1197122072889190)
                          (0.13	0.1296341426196950)
                          (0.14	0.1395431146442360)
                          (0.15	0.1494381324735990)
                          (0.16	0.1593182066142460)
                          (0.17	0.1691823490669960)
                          (0.18	0.1790295734258240)
                          (0.19	0.1888588949765010)
                          (0.20	0.1986693307950610)
                          (0.21	0.2084598998461000)
                          (0.22	0.2182296230808690)
                          (0.23	0.2279775235351880)
                          (0.24	0.2377026264271350)
                          (0.25	0.2474039592545230)
                          (0.26	0.2570805518921550)
                          (0.27	0.2667314366888310)
                          (0.28	0.2763556485641140)
                          (0.29	0.2859522251048360)
                          (0.30	0.2955202066613400)
                          (0.31	0.3050586364434430)
                          (0.32	0.3145665606161180)
                          (0.33	0.3240430283948680)
                          (0.34	0.3334870921408140)
                          (0.35	0.3428978074554510)
                          (0.36	0.3522742332750900)
                          (0.37	0.3616154319649620)
                          (0.38	0.3709204694129830)
                          (0.39	0.3801884151231610)
                          (0.40	0.3894183423086510)
                          (0.41	0.3986093279844230)
                          (0.42	0.4077604530595700)
                          (0.43	0.4168708024292110)
                          (0.44	0.4259394650660000)
                          (0.45	0.4349655341112300)
                          (0.46	0.4439481069655200)
                          (0.47	0.4528862853790680)
                          (0.48	0.4617791755414830)
                          (0.49	0.4706258881711580)
                          (0.50	0.4794255386042030)
                          (0.51	0.4881772468829070)
                          (0.52	0.4968801378437370)
                          (0.53	0.5055333412048470)
                          (0.54	0.5141359916531130)
                          (0.55	0.5226872289306590)
                          (0.56	0.5311861979208830)
                          (0.57	0.5396320487339690)
                          (0.58	0.5480239367918740)
                          (0.59	0.5563610229127840)
                          (0.60	0.5646424733950350)
                          (0.61	0.5728674601004810)
                          (0.62	0.5810351605373050)
                          (0.63	0.5891447579422700)
                          (0.64	0.5971954413623920)
                          (0.65	0.6051864057360400)
                          (0.66	0.6131168519734340)
                          (0.67	0.6209859870365600)
                          (0.68	0.6287930240184690)
                          (0.69	0.6365371822219680)
                          (0.70	0.6442176872376910)
                          (0.71	0.6518337710215370)
                          (0.72	0.6593846719714730)
                          (0.73	0.6668696350036980)
                          (0.74	0.6742879116281450)
                          (0.75	0.6816387600233340)
                          (0.76	0.6889214451105510)
                          (0.77	0.6961352386273570)
                          (0.78	0.7032794192004100)
                          (0.79	0.7103532724176080)
                          (0.80	0.7173560908995230)
                          (0.81	0.7242871743701430)
                          (0.82	0.7311458297268960)
                          (0.83	0.7379313711099630)
                          (0.84	0.7446431199708590)
                          (0.85	0.7512804051402930)
                          (0.86	0.7578425628952770)
                          (0.87	0.7643289370255050)
                          (0.88	0.7707388788989690)
                          (0.89	0.7770717475268240)
                          (0.90	0.7833269096274830)
                          (0.91	0.7895037396899500)
                          (0.92	0.7956016200363660)
                          (0.93	0.8016199408837770)
                          (0.94	0.8075581004051140)
                          (0.95	0.8134155047893740)
                          (0.96	0.8191915683009980)
                          (0.97	0.8248857133384500)
                          (0.98	0.8304973704919700)
                          (0.99	0.8360259786005210)
                          (1.00	0.8414709848078970)
                          (1.01	0.8468318446180150)
                          (1.02	0.8521080219493630)
                          (1.03	0.8572989891886030)
                          (1.04	0.8624042272433380)
                          (1.05	0.8674232255940170)
                          (1.06	0.8723554823449860)
                          (1.07	0.8772005042746820)
                          (1.08	0.8819578068849480)
                          (1.09	0.8866269144494870)
                          (1.10	0.8912073600614350)
                          (1.11	0.8956986856800480)
                          (1.12	0.9001004421765050)
                          (1.13	0.9044121893788260)
                          (1.14	0.9086334961158830)
                          (1.15	0.9127639402605210)
                          (1.16	0.9168031087717670)
                          (1.17	0.9207505977361360)
                          (1.18	0.9246060124080200)
                          (1.19	0.9283689672491670)
                          (1.20	0.9320390859672260)
                          (1.21	0.9356160015533860)
                          (1.22	0.9390993563190680)
                          (1.23	0.9424888019316970)
                          (1.24	0.9457839994495390)
                          (1.25	0.9489846193555860)
                          (1.26	0.9520903415905160)
                          (1.27	0.9551008555846920)
                          (1.28	0.9580158602892250)
                          (1.29	0.9608350642060730)
                          (1.30	0.9635581854171930)
                          (1.31	0.9661849516127340)
                          (1.32	0.9687151001182650)
                          (1.33	0.9711483779210450)
                          (1.34	0.9734845416953190)
                          (1.35	0.9757233578266590)
                          (1.36	0.9778646024353160)
                          (1.37	0.9799080613986140)
                          (1.38	0.9818535303723600)
                          (1.39	0.9837008148112770)
                          (1.40	0.9854497299884600)
                          (1.41	0.9871001010138500)
                          (1.42	0.9886517628517200)
                          (1.43	0.9901045603371780)
                          (1.44	0.9914583481916860)
                          (1.45	0.9927129910375880)
                          (1.46	0.9938683634116450)
                          (1.47	0.9949243497775810)
                          (1.48	0.9958808445376400)
                          (1.49	0.9967377520431430)
                          (1.50	0.9974949866040540)
                          (1.51	0.9981524724975480)
                          (1.52	0.9987101439755830)
                          (1.53	0.9991679452714760)
                          (1.54	0.9995258306054790)
                          (1.55	0.9997837641893570)
                          (1.56	0.9999417202299660)
                          (1.57	0.9999996829318350))))
      (letrec ((fn (lambda (acc i)
                      (if (> i 157)
                          (sqrt (/ acc i))
                          (let* ((v (vector-ref test-vector i))
                                 (d (- (sin (car v)) (cadr v))))
                            (fn (+ acc (* d d)) (+ i 1)))))))
          (let ((rms-error (fn 0 0)))
            #; (display-all "rms error: " rms-error)
            (assert
              (< rms-error 1e-14)
              " expect rms error to be less than 1e-14, got " rms-error))))))

  (test-case "asin x - test vectors over 0 ≤ x ≤ 1" (lambda ()
    (let ((test-vector #( (0.00	0.0000000000000000)
                          (0.01	0.0100001666741671)
                          (0.02	0.0200013335733905)
                          (0.03	0.0300045018234769)
                          (0.04	0.0400106743539889)
                          (0.05	0.0500208568057700)
                          (0.06	0.0600360584452784)
                          (0.07	0.0700572930880503)
                          (0.08	0.0800855800336590)
                          (0.09	0.0901219450145953)
                          (0.10	0.1001674211615600)
                          (0.11	0.1102230499877470)
                          (0.12	0.1202898823947880)
                          (0.13	0.1303689797031460)
                          (0.14	0.1404614147098560)
                          (0.15	0.1505682727766860)
                          (0.16	0.1606906529519110)
                          (0.17	0.1708296691291050)
                          (0.18	0.1809864512465480)
                          (0.19	0.1911621465310600)
                          (0.20	0.2013579207903310)
                          (0.21	0.2115749597580960)
                          (0.22	0.2218144704967940)
                          (0.23	0.2320776828627130)
                          (0.24	0.2423658510389630)
                          (0.25	0.2526802551420790)
                          (0.26	0.2630222029084690)
                          (0.27	0.2733930314674730)
                          (0.28	0.2837941092083280)
                          (0.29	0.2942268377489820)
                          (0.30	0.3046926540153980)
                          (0.31	0.3151930324407240)
                          (0.32	0.3257294872946300)
                          (0.33	0.3363035751539800)
                          (0.34	0.3469168975271620)
                          (0.35	0.3575711036455100)
                          (0.36	0.3682678934366400)
                          (0.37	0.3790090206959510)
                          (0.38	0.3897962964742610)
                          (0.39	0.4006315927013720)
                          (0.40	0.4115168460674880)
                          (0.41	0.4224540621867560)
                          (0.42	0.4334453200698860)
                          (0.43	0.4444927769358190)
                          (0.44	0.4555986733958230)
                          (0.45	0.4667653390472960)
                          (0.46	0.4779951985189520)
                          (0.47	0.4892907780141160)
                          (0.48	0.5006547124045880)
                          (0.49	0.5120897529341480)
                          (0.50	0.5235987755982990)
                          (0.51	0.5351847902756000)
                          (0.52	0.5468509506959440)
                          (0.53	0.5586005653428010)
                          (0.54	0.5704371093999220)
                          (0.55	0.5823642378687430)
                          (0.56	0.5943858000010620)
                          (0.57	0.6065058552130870)
                          (0.58	0.6187286906722510)
                          (0.59	0.6310588407780210)
                          (0.60	0.6435011087932840)
                          (0.61	0.6560605909249230)
                          (0.62	0.6687427032023720)
                          (0.63	0.6815532115631170)
                          (0.64	0.6944982656265560)
                          (0.65	0.7075844367253560)
                          (0.66	0.7208187608700900)
                          (0.67	0.7342087874533590)
                          (0.68	0.7477626346599210)
                          (0.69	0.7614890527476330)
                          (0.70	0.7753974966107530)
                          (0.71	0.7894982093461720)
                          (0.72	0.8038023189330300)
                          (0.73	0.8183219506315600)
                          (0.74	0.8330703583416480)
                          (0.75	0.8480620789814810)
                          (0.76	0.8633131150155540)
                          (0.77	0.8788411516685800)
                          (0.78	0.8946658172342350)
                          (0.79	0.9108089974073980)
                          (0.80	0.9272952180016120)
                          (0.81	0.9441521151541560)
                          (0.82	0.9614110187641020)
                          (0.83	0.9791076843683520)
                          (0.84	0.9972832223718000)
                          (0.85	1.0159852938148300)
                          (0.86	1.0352696724805100)
                          (0.87	1.0552023205488100)
                          (0.88	1.0758622004540000)
                          (0.89	1.0973451695228300)
                          (0.90	1.1197695149986300)
                          (0.91	1.1432840618500300)
                          (0.92	1.1680804852142400)
                          (0.93	1.1944128444771700)
                          (0.94	1.2226303055219400)
                          (0.95	1.2532358975033800)
                          (0.96	1.2870022175865700)
                          (0.97	1.3252308092796000)
                          (0.98	1.3704614844717800)
                          (0.99	1.4292568534704700)
                          (1.00	1.5707963267949000))))
      (letrec ((fn (lambda (acc i)
                      (if (> i 100)
                          (sqrt (/ acc i))
                          (let* ((v (vector-ref test-vector i))
                                 (d (- (asin (car v)) (cadr v))))
                            (fn (+ acc (* d d)) (+ i 1)))))))
          (let ((rms-error (fn 0 0)))
            #; (display-all "rms error: " rms-error)
            (assert
              (< rms-error 1e-14)
              " expect rms error to be less than 1e-14, got " rms-error))))))

  (test-case "branch cuts for atan2" (lambda ()
    ; y = +0  x > 0   Just above positive x-axis  +0
    (assert-equal (atan 0.0 0.5) 0)
    ; y > 0   x > 0   Quadrant I                  +0 < result < π/2
    (assert (> (atan 0.25 0.5) 0))
    (assert (< (atan 0.25 0.5) (/ pi 2)))
    (assert (> (atan 0.5 0.25) 0))
    (assert (< (atan 0.5 0.25) (/ pi 2)))
    ; y > 0   x = ±0  Positive y-axis             π/2
    (assert-equal (atan 0.5 0.0) (/ pi 2))
    (assert-equal (atan 0.5 -0.0) (/ pi 2))
    ; y > 0   x < 0   Quadrant II                 π/2 < result < π
    (assert (> (atan 0.25 -0.5) (/ pi 2)))
    (assert (< (atan 0.25 -0.5) pi))
    (assert (> (atan 0.5 -0.25) (/ pi 2)))
    (assert (< (atan 0.5 -0.25) pi))
    ; y = +0  x < 0   Just above negative x-axis  π
    (assert-equal (atan 0.0 -0.5) pi)
    ; y = −0  x < 0   Just below negative x-axis  -π
    (assert-equal (atan -0.0 -0.5) (- pi))
    ; y < 0   x < 0   Quadrant III                −π < result < −π/2
    (assert (> (atan -0.25 -0.5) (- pi)))
    (assert (< (atan -0.25 -0.5) (- (/ pi 2))))
    (assert (> (atan -0.5 -0.25) (- pi)))
    (assert (< (atan -0.5 -0.25) (- (/ pi 2))))
    ; y < 0   x = ±0  Negative y-axis             −π/2
    (assert-equal (atan -0.5 0.0) (- (/ pi 2)))
    (assert-equal (atan -0.5 -0.0) (- (/ pi 2)))
    ; y < 0   x > 0   Quadrant IV                 −π/2 < result < −0
    (assert (> (atan -0.25 0.5) (- (/ pi 2))))
    (assert (< (atan -0.25 0.5) 0))
    (assert (> (atan -0.5 0.25) (- (/ pi 2))))
    (assert (< (atan -0.5 0.25) 0))
    ; y = −0  x > 0   Just below positive x-axis  −0
    (assert-equal (atan -0.0 0.5) -0.0)
    (assert-equal (number->string (atan -0.0 0.5)) "-0")
    ; y = +0  x = +0  Near origin                 +0
    (assert-equal (atan 0.0 0.0) 0)
    (assert-equal (number->string (atan 0.0 0.0)) "0")
    ; y = −0  x = +0  Near origin                 −0
    (assert-equal (atan -0.0 0.0) -0.0)
    (assert-equal (number->string (atan -0.0 0.0)) "-0")
    ; y = +0  x = −0  Near origin                 π
    (assert-equal (atan 0.0 -0.0) pi)
    ; y = −0  x = −0  Near origin                 −π
    (assert-equal (atan -0.0 -0.0) (- pi))
  ))
)
