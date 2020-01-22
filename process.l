(setq *OFST 0)

(de RD (N)
    (setq *OFST (+ *OFST N))
    (rd N))


(de rd-str (n)
    (pack (make
        (do n (link (char (RD 1)))))))

(de rd-hdr (N)
    (let (L (RD 4) # Length must be 6
          F (RD 2) # Format 0 => Single Track, 1 => 1 or more parallel tracks, 2 => 1 or more independent tracks
          T (RD 2) # Number of tracks
          D (RD 2) # division (time related)
          DT (bit? (>> -15 1) D)
          TPQ (unless DT D) # Ticks per quarter note
          (FPS TPF) (when DT (list
                        (>> 8 (& D (hex "7f00"))) # Frames per second
                        (& D (hex "ff"))))) # Ticks per frame
        (list N L F T D DT TPQ FPS TPF)))

(de rd-varlen ()
    (let (B 128
          R 0)
         (while (= 128 (& 128 B))
             (setq B (RD 1))
             (setq R (| (>> -7 R) (& B 127))))
         R))

(de rd-vstr ()
    (let N (rd-varlen)
    (pack (make
        (do N (link (char (RD 1))))))))
        
(de rd-ln&bts ()
    (let L (rd-varlen)
         (RD L)))

(de bits->event-type (Bts)
  (cond
    ((= Bts (hex "8")) 'note-off)
    ((= Bts (hex "9")) 'note-on)
    ((= Bts (hex "a")) 'aftertouch)
    ((= Bts (hex "b")) 'control-change)
    ((= Bts (hex "c")) 'program-change)
    ((= Bts (hex "d")) 'channel-aftertouch)
    ((= Bts (hex "e")) 'pitch-bend)
    (T (pack "Uknown message" Bts))
  )
)

(de two-byte-event? (Bits)
  (not (or (= Bits (hex "c")) (= Bits (hex "d")))))


(setq *CTR 0)

(de rd-evt ()
  (let B1 (RD 1)
    (cond
        ((= B1 (hex "ff")) #meta
          (list 'meta
            (let B2 (RD 1)
                 (cond
                       ((= B2 1) (list 'text (rd-vstr)))
                       ((= B2 2) (list 'copyright (rd-vstr)))
                       ((= B2 3) (list 'track-name (rd-vstr)))
                       ((= B2 (hex "2f")) (list 'end-of-track (rd-vstr)))
                       ((= B2 (hex "51")) (list 'set-tempo (RD (rd-varlen))))
                       ((= B2 (hex "58")) (list 'time-signature (make (do (rd-varlen) (link (RD 1))) )))
                       ((= B2 (hex "59")) (RD 1) (list 'key-signature (RD 1) (if (= 0 (RD 1)) 'major 'minor) ))
                       (T (list 'unknown B2 (rd-vstr)))

                 )
            )
          )
        )
        ((= B1 (hex "f0")) (list 'sysex (rd-vstr)))
        (T 
            (cond
               ((not (= 0 (and (hex "80") B1)))
                     (let (
                            Chn (& 7 B1)
                            MN  (>> 4 B1)
                            Msg (bits->event-type MN)
                            P1 (RD 1)
                            P2 (if (two-byte-event? MN) (RD 1) NIL)

                          )
                          (setq *CTR (inc *CTR))
                          (list Msg Chn P1 P2)
                     )
               )
               (T (prinl "DONE") (bye))
            )
        )
        (T (setq *OFST 10000000))
    )
  )
)

(de rd-trk ()
    (let (L (RD 4)
          E (+ *OFST L))
              (make
                (let TIME 0
                    (while (< *OFST E)
                         (let (DT (rd-varlen)
                               E (rd-evt))
                               (setq TIME (+ TIME DT))
                               (link (cons TIME E))
                          )
                    )
                )
              )
    )
)


            #         (let (D (rd-varlen)
            #               )
            #               (setq R 1000)
            #               (link D))))))
          
(de rd-chunk ()
    (let (N (rd-str 4)) # MThd
         (case N
            ("MThd" (rd-hdr N))
            ("MTrk" (rd-trk))
            (T NIL))))
         


(setq *FN "beatit.mid")
          
(when *FN
    (in *FN
       (while (rd-chunk) (prinl (pretty @)))))


(bye)
