; 名詞句に形容詞を追加する

(define adjectives '(adjective big small green round)) 

(define (parse-noun-phrase)
    (define (maybe-extend noun-phrase)
        (amb noun-phrase
            (maybe-extend (list 'noun-phrase
                                noun-phrase
                                (parse-prepositional-phrase)))))
    (maybe-extend (parse-simple-noun-phrase)))

(define (parse-simple-noun-phrase)
    (list 'simple-noun-phrase
          (parse-article-phrase)
          (parse-word nouns)))

(define (parse-article-phrase)
    (define (maybe-extend article-phrase)
        (amb article-phrase
             (maybe-extend (list 'article-phrase
                                 article-phrase
                                 (parse-word adjectives)))))
    (maybe-extend (parse-word articles)))

