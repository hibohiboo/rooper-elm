### swiperヒストリAPI

```elm
mainContent : Html Msg
mainContent =
    Swiper.wrapper
        [ div [ class "swiper-slide", attribute "data-history" "main" ] [ mainContentFirst ]
        , div [ class "swiper-slide", attribute "data-history" "create-room" ] [ createRoomView ]
        ]
```

## 便利なelmやつ
https://package.elm-lang.org/packages/elm-community/maybe-extra/5.1.0/Maybe-Extra

```
combine : List (Maybe a) -> Maybe (List a)
```

Maybeのリストを扱うときに便利。
