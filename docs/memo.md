### swiperヒストリAPI

```elm
mainContent : Html Msg
mainContent =
    Swiper.wrapper
        [ div [ class "swiper-slide", attribute "data-history" "main" ] [ mainContentFirst ]
        , div [ class "swiper-slide", attribute "data-history" "create-room" ] [ createRoomView ]
        ]
```

