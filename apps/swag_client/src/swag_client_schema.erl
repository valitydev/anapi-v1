%% -*- mode: erlang -*-
-module(swag_client_schema).

-export([get/0]).
-export([get_raw/0]).
-export([enumerate_discriminator_children/1]).

-define(DEFINITIONS, <<"definitions">>).

-spec get() -> swag_client:object().
get() ->
    ct_expand:term(enumerate_discriminator_children(maps:with([?DEFINITIONS], get_raw()))).

-spec enumerate_discriminator_children(Schema :: map()) ->
    Schema :: map() | no_return().
enumerate_discriminator_children(Schema = #{?DEFINITIONS := Defs}) ->
    try
        Parents = enumerate_parents(Defs),
        DefsFixed = maps:fold(fun correct_definition/3, Defs, Parents),
        Schema#{?DEFINITIONS := DefsFixed}
    catch
        _:Error ->
            handle_error(Error)
    end;
enumerate_discriminator_children(_) ->
    handle_error(no_definitions).

-spec handle_error(_) ->
    no_return().
handle_error(Error) ->
    erlang:error({schema_invalid, Error}).

enumerate_parents(Definitions) ->
    maps:fold(
        fun
            (Name, #{<<"allOf">> := AllOf}, AccIn) ->
                lists:foldl(
                    fun
                        (#{<<"$ref">> := <<"#/definitions/", Parent/binary>>}, Acc) ->
                            Schema = maps:get(Parent, Definitions),
                            Discriminator = maps:get(<<"discriminator">>, Schema, undefined),
                            add_parent_child(Discriminator, Parent, Name, Acc);
                        (_Schema, Acc) ->
                            Acc
                    end,
                    AccIn,
                    AllOf
                );
            (Name, #{<<"discriminator">> := _}, Acc) ->
                add_parent(Name, Acc);
            (_Name, _Schema, AccIn) ->
                AccIn
        end,
        #{},
        Definitions
    ).

add_parent_child(undefined, _Parent, _Child, Acc) ->
    Acc;
add_parent_child(_Discriminator, Parent, Child, Acc) ->
    maps:put(Parent, [Child | maps:get(Parent, Acc, [])], Acc).

add_parent(Parent, Acc) when not is_map_key(Parent, Acc) ->
    maps:put(Parent, [], Acc);
add_parent(_Parent, Acc) ->
    Acc.

correct_definition(Parent, Children, Definitions) ->
    ParentSchema1 = maps:get(Parent, Definitions),
    Discriminator = maps:get(<<"discriminator">>, ParentSchema1),
    ParentSchema2 = deep_put([<<"properties">>, Discriminator, <<"enum">>], Children, ParentSchema1),
    maps:put(Parent, ParentSchema2, Definitions).

deep_put([K], V, M) ->
    M#{K => V};
deep_put([K | Ks], V, M) ->
    maps:put(K, deep_put(Ks, V, maps:get(K, M)), M).

-spec get_raw() -> map().
get_raw() ->
    #{
  <<"swagger">> => <<"2.0">>,
  <<"info">> => #{
    <<"description">> => <<"## Описание\nValitydev Analytics API является точкой взаимодействия с аналитической и поисковой частью платформы. Все аналитическоие запросы осуществляются с помощью вызовов соответствующих методов API. Любые сторонние приложения, включая наши веб-сайты, личные кабинеты и другие UI-интерфейсы являются внешними приложениями-клиентами.\nValitydev Analytics API работает поверх HTTP-протокола. Мы используем REST архитектуру, схема описывается в соответствии со [Swagger](http://swagger.io/). Коды возврата описываются соответствующими HTTP-статусами. Платформа принимает и возвращает JSON-структуры в HTTP body.\n## Запросы\nЛюбой вызов методов API обязан предваряться предоставлением уникального для участника в пределах платформы ID запроса. Данный ID передается в соответствующем заголовке HTTP-запроса:\n```\n X-Request-ID: oX5MWM2AQy\n```\nПлатформа гарантирует идемпотентность запросов, отправленных с одинаковым ID.\n## Тип содержимого и кодировка\nЛюбой запрос к API должен выполняться в кодировке UTF-8 и с указанием содержимого в формате JSON\n```\n  Content-Type: application/json; charset=utf-8\n```\n## Формат дат\nПлатформа принимает значения date-time в стандарте ISO 8601 с обязательным указанием UTC-оффсета, например:\n```\n  2017-01-01T00:00:00Z\n  2017-01-01T00:00:01+00:00\n```\n## Максимальное время обработки запроса\nК любому вызову методов API можно добавить параметр отсечки по времени, определяющий максимальное время ожидания завершения операции по запросу. Данная отсечка передается в соответствующем заголовке HTTP-запроса:\n```\n X-Request-Deadline: 10s\n```\nЗначение отсечки может быть задано в формате ISO 8601 (см. [Формат дат](#section/Format-dat)), либо в относительных величинах, например:\n`150000ms`, `540s`, `3.5m` При этом возможные единицы измерения `ms`, `s`, `m`. В обоих случаях  не рекомендуется, чтобы задаваемое значение было меньше **3 секунд** и превышало **1 минуту**.\n## Поиск по магазинам\nAPI предоставляет несколько различных критериев для выбора магазинов, в рамках которых будет выполняться поиск или аналитика: `shopID`, `shopIDs`, `paymentInstitutionRealm`. В случае использования нескольких критериев одновременно в выборку будут включены магазины, подпадающие под хотя бы один из перечисленных критериев.\n">>,
    <<"version">> => <<"1.0.0">>,
    <<"title">> => <<"Valitydev Platform Analytics API">>,
    <<"termsOfService">> => <<"https://vality.dev/">>
  },
  <<"basePath">> => <<"/lk/v1">>,
  <<"tags">> => [ #{
    <<"name">> => <<"Search">>,
    <<"description">> => <<"Для получения списка всех инвойсов/платежей указанного магазина необходимо вызвать соответствующий метод платформы. Имеется возможность отфильтровать выборку по определенным статусам.\n">>,
    <<"x-displayName">> => <<"Поиск">>
  }, #{
    <<"name">> => <<"Error Codes">>,
    <<"description">> => <<"\n## Ошибки бизнес-логики\nВсе ошибки бизнес-логики имеют следуюший вид:\n\n  ```json\n  {\n    \"code\": \"string\",\n    \"message\": \"string\"\n  }\n  ```\n\nВ поле `code` содержится тип ошибки, а в `message` - дополнительная информация по произошедшей ошибке.\nНа данный момент существуют следующие коды ошибок:\n\n  | Код                              | Описание                                                                                                                              |\n  | ---                              | --------                                                                                                                              |\n  | **invalidDeadline**              | Неверный формат максимального времени обработки запроса.                                                                              |\n  | **ambiguousPartyID**             | Невозможно однозначно определить идентификатор участника, укажите идентификатор в запросе явно.                                       |\n  | **invalidPartyID**               | Участник с указанным идентификатором не существует или недоступен.                                                                    |\n  | **invalidRequest**               | Прочие неверные данные запроса.                                                                                                       |\n\n## Общие ошибки\nОшибки возникающие при попытках совершения операций с незарегистрированными в системе объектами. Имеют вид\n\n  ```json\n  {\n      \"message\": \"string\"\n  }\n  ```\n\nВ поле `message` содержится информация по произошедшей ошибке. Например:\n\n  ```json\n  {\n      \"message\": \"Invalid token\"\n  }\n  ```\n\n## Ошибки обработки запросов\nВ процессе обработки запросов силами нашей платформы могут происходить различные непредвиденные ситуации. Об их появлении платформа сигнализирует по протоколу HTTP соответствующими [статусами][5xx], обозначающими ошибки сервера.\n\n  |  Код    |  Описание  |\n  | ------- | ---------- |\n  | **500** | В процессе обработки платформой запроса возникла непредвиденная ситуация. При получении подобного кода ответа мы рекомендуем обратиться в техническую поддержку. |\n  | **503** | Платформа временно недоступна и не готова обслуживать данный запрос. Запрос гарантированно не выполнен, при получении подобного кода ответа попробуйте выполнить его позднее, когда доступность платформы будет восстановлена. |\n  | **504** | Платформа превысила допустимое время обработки запроса, результат запроса не определён. Попробуйте отправить запрос повторно или выяснить результат выполнения исходного запроса, если повторное исполнение запроса нежелательно. |\n\n[5xx]: https://tools.ietf.org/html/rfc7231#section-6.6\n\nЕсли вы получили ошибку, которой нет в данном описании, обратитесь в техническую поддержку.\n">>,
    <<"x-displayName">> => <<"Коды ошибок">>
  }, #{
    <<"name">> => <<"Reports">>,
    <<"description">> => <<"Один раз в отчетный период платформа автоматически подготавливает и размещает документы в формате XLSX с разбиением по магазину активной категории. Также, каждый документ будет подписан [квалифицированной ЭЦП](http://minsvyaz.ru/ru/appeals/faq/31/). Данная подпись является юридически значимой и позволяет полностью отказаться от бумажного документооборота.\n">>,
    <<"x-displayName">> => <<"Отчеты">>
  }, #{
    <<"name">> => <<"Analitics">>,
    <<"description">> => <<"Аналитические методы по статистике платежей\n">>,
    <<"x-displayName">> => <<"Аналитика">>
  } ],
  <<"schemes">> => [ <<"https">> ],
  <<"consumes">> => [ <<"application/json; charset=utf-8">> ],
  <<"produces">> => [ <<"application/json; charset=utf-8">> ],
  <<"security">> => [ #{
    <<"bearer">> => [ ]
  } ],
  <<"paths">> => #{
    <<"/analytics/balances/current-shop-balances">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Analytics">> ],
        <<"description">> => <<"Получение текущего баланса с группировкой по магазинам">>,
        <<"operationId">> => <<"getCurrentBalancesGroupByShop">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"excludeShopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов не включаемые в запрос">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Список балансов с группировкой по магазинам">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/analytics/balances/current">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Analytics">> ],
        <<"description">> => <<"Получение текущего баланса по магазину">>,
        <<"operationId">> => <<"getCurrentBalances">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"excludeShopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов не включаемые в запрос">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Список оборотов с группировкой по валютам">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_1">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/analytics/payments-tool">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Analytics">> ],
        <<"description">> => <<"Получение распределения использования платежных инструментов">>,
        <<"operationId">> => <<"getPaymentsToolDistribution">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"excludeShopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов не включаемые в запрос">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Распределение использования платежных инструментов">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_2">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/analytics/payments/amount">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Analytics">> ],
        <<"description">> => <<"Получение списка оборотов с группировкой по валютам">>,
        <<"operationId">> => <<"getPaymentsAmount">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"excludeShopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов не включаемые в запрос">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Список оборотов с группировкой по валютам">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_1">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/analytics/payments/average">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Analytics">> ],
        <<"description">> => <<"Получение среднего размера платежа с группировкой по валютам">>,
        <<"operationId">> => <<"getAveragePayment">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"excludeShopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов не включаемые в запрос">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Средний размер платежа с группировкой по валютам">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_1">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/analytics/payments/count">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Analytics">> ],
        <<"description">> => <<"Получение количества платежей с группировкой по валютам">>,
        <<"operationId">> => <<"getPaymentsCount">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"excludeShopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов не включаемые в запрос">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Среднее количество платежей с группировкой по валютам">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_3">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/analytics/payments/errors">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Analytics">> ],
        <<"description">> => <<"Получение распределения ошибок">>,
        <<"operationId">> => <<"getPaymentsErrorDistribution">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"excludeShopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов не включаемые в запрос">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Распределение ошибок">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_4">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/analytics/payments/split-amount">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Analytics">> ],
        <<"description">> => <<"Получение списка оборотов с группировкой по валютам и разделенные по временным интервалам">>,
        <<"operationId">> => <<"getPaymentsSplitAmount">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"excludeShopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов не включаемые в запрос">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"splitUnit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Единица времени сегмента разбиения">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"minute">>, <<"hour">>, <<"day">>, <<"week">>, <<"month">>, <<"year">> ]
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Список оборотов с группировкой по валютам и разделенные по временным интервалам">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_5">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/analytics/payments/split-count">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Analytics">> ],
        <<"description">> => <<"Получение количества платежей с группировкой по валютам и статусам, разделенного по временным интервалам">>,
        <<"operationId">> => <<"getPaymentsSplitCount">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"excludeShopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов не включаемые в запрос">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"splitUnit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Единица времени сегмента разбиения">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"minute">>, <<"hour">>, <<"day">>, <<"week">>, <<"month">>, <<"year">> ]
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Количество платежей с группировкой по валютам и статусам, разделенное по временным интервалам">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_6">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/analytics/payments/sub-errors">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Analytics">> ],
        <<"description">> => <<"Получение распределения ошибок с подошибками">>,
        <<"operationId">> => <<"getPaymentsSubErrorDistribution">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"excludeShopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов не включаемые в запрос">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Распределение ошибок с подошибками">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_7">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/analytics/refunds/amount">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Analytics">> ],
        <<"description">> => <<"Получение списка возвратов с группировкой по валютам">>,
        <<"operationId">> => <<"getRefundsAmount">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"excludeShopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов не включаемые в запрос">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Список возвратов с группировкой по валютам">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_1">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/chargebacks">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Search">> ],
        <<"description">> => <<"Поиск чарджбэков">>,
        <<"operationId">> => <<"searchChargebacks">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор магазина">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Лимит выборки">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"offset">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Смещение выборки">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"minimum">> => 0
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор инвойса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"paymentID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор платежа">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"chargebackID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор чарджбэка">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"chargebackStatuses">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Статусы чарджбэков">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>,
            <<"enum">> => [ <<"pending">>, <<"accepted">>, <<"rejected">>, <<"cancelled">> ]
          }
        }, #{
          <<"name">> => <<"chargebackStages">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Этапы чарджбэков">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>,
            <<"enum">> => [ <<"chargeback">>, <<"pre_arbitration">>, <<"arbitration">> ]
          }
        }, #{
          <<"name">> => <<"chargebackCategories">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Категории чарджбэков">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>,
            <<"enum">> => [ <<"fraud">>, <<"dispute">>, <<"authorisation">>, <<"processing_error">> ]
          }
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Найденные чарджбэки">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_8">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/invoices">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Search">> ],
        <<"description">> => <<"Поиск инвойсов">>,
        <<"operationId">> => <<"searchInvoices">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор магазина">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Лимит выборки">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"invoiceIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Список инвойсов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"invoiceStatus">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Статус инвойса для поиска">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"unpaid">>, <<"cancelled">>, <<"paid">>, <<"fulfilled">> ]
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор инвойса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"externalID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Внешний идентификатор">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"invoiceAmountFrom">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Минимальная сумма инвойса">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"minimum">> => 1,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"invoiceAmountTo">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Максимальная сумма инвойса">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"minimum">> => 1,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"excludedShops">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов, исключаемых из поиска">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Найденные инвойсы">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_9">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/payments">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Search">> ],
        <<"description">> => <<"Поиск платежей">>,
        <<"operationId">> => <<"searchPayments">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор магазина">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Лимит выборки">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"invoiceIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Список инвойсов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentStatus">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Статус платежа для поиска">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"pending">>, <<"processed">>, <<"captured">>, <<"cancelled">>, <<"refunded">>, <<"failed">> ]
        }, #{
          <<"name">> => <<"paymentFlow">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Flow платежа">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"instant">>, <<"hold">> ]
        }, #{
          <<"name">> => <<"paymentMethod">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Метод оплаты">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"bankCard">>, <<"paymentTerminal">> ]
        }, #{
          <<"name">> => <<"paymentTerminalProvider">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Провайдер платежного терминала">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"euroset">>, <<"wechat">>, <<"alipay">>, <<"zotapay">>, <<"qps">>, <<"uzcard">>, <<"rbs">> ]
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор инвойса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"paymentID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор платежа">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"externalID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Внешний идентификатор">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"payerEmail">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Email, указанный при оплате">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 100,
          <<"format">> => <<"email">>
        }, #{
          <<"name">> => <<"payerIP">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"IP-адрес плательщика">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 45,
          <<"format">> => <<"ip-address">>
        }, #{
          <<"name">> => <<"payerFingerprint">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный отпечаток user agent'а плательщика">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 1000
        }, #{
          <<"name">> => <<"customerID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор плательщика">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"first6">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Первые 6 цифр номера карты">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^\\d{6}$">>
        }, #{
          <<"name">> => <<"last4">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Последние 4 цифры номера карты">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^\\d{4}$">>
        }, #{
          <<"name">> => <<"rrn">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Retrieval Reference Number">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^[a-zA-Z0-9]{12}$">>
        }, #{
          <<"name">> => <<"approvalCode">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Authorization Approval Code">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"bankCardTokenProvider">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Провайдер платежных токенов">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"applepay">>, <<"googlepay">>, <<"samsungpay">>, <<"yandexpay">> ]
        }, #{
          <<"name">> => <<"bankCardPaymentSystem">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Платежная система">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"visa">>, <<"mastercard">>, <<"visaelectron">>, <<"maestro">>, <<"forbrugsforeningen">>, <<"dankort">>, <<"amex">>, <<"dinersclub">>, <<"discover">>, <<"unionpay">>, <<"jcb">>, <<"nspkmir">>, <<"elo">>, <<"rupay">>, <<"dummy">>, <<"uzcard">> ]
        }, #{
          <<"name">> => <<"paymentAmountFrom">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Минимальная сумма платежа">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"minimum">> => 1,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"paymentAmountTo">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Максимальная сумма платежа">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"minimum">> => 1,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"excludedShops">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов, исключаемых из поиска">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Найденные платежи">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_10">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/payouts">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Search">> ],
        <<"description">> => <<"Поиск выплат">>,
        <<"operationId">> => <<"searchPayouts">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор магазина">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Лимит выборки">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"offset">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Смещение выборки">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"minimum">> => 0
        }, #{
          <<"name">> => <<"payoutID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор выплаты">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"payoutToolType">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип выплаты для поиска\n  * PayoutAccount - выплата на банковский счёт\n  * Wallet - выплата на кошелёк\n  * PaymentInstitutionAccount - выплата на счёт платежной организации\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"PayoutAccount">>, <<"Wallet">>, <<"PaymentInstitutionAccount">> ]
        }, #{
          <<"name">> => <<"excludedShops">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов, исключаемых из поиска">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Найденные выплаты">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_11">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/refunds">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Search">> ],
        <<"description">> => <<"Поиск возвратов">>,
        <<"operationId">> => <<"searchRefunds">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор магазина">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Лимит выборки">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"offset">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Смещение выборки">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"minimum">> => 0
        }, #{
          <<"name">> => <<"invoiceIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Список инвойсов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"invoiceID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор инвойса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"paymentID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор платежа">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"refundID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор возврата">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"externalID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Внешний идентификатор">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"refundStatus">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Статус возврата">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"pending">>, <<"succeeded">>, <<"failed">> ]
        }, #{
          <<"name">> => <<"excludedShops">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов, исключаемых из поиска">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Найденные возвраты">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_12">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/reports">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Reports">> ],
        <<"description">> => <<"Получить список отчетов по данному магазину за период">>,
        <<"operationId">> => <<"searchReports">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор магазина">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopIDs">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификаторы магазинов">>,
          <<"required">> => false,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>
          }
        }, #{
          <<"name">> => <<"paymentInstitutionRealm">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"live">>, <<"test">> ]
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"limit">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Лимит выборки">>,
          <<"required">> => false,
          <<"type">> => <<"integer">>,
          <<"maximum">> => 1000,
          <<"minimum">> => 1,
          <<"format">> => <<"int32">>
        }, #{
          <<"name">> => <<"reportTypes">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Типы отчетов">>,
          <<"required">> => true,
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"type">> => <<"string">>,
            <<"enum">> => [ <<"provisionOfService">>, <<"paymentRegistry">>, <<"paymentRegistryByPayout">> ]
          }
        }, #{
          <<"name">> => <<"continuationToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>,
          <<"required">> => false,
          <<"type">> => <<"string">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Найденные отчеты">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_200_13">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Ошибочные данные для получения">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"Reports">> ],
        <<"description">> => <<"Сгенерировать отчет с указанным типом по магазину за указанный промежуток времени">>,
        <<"operationId">> => <<"createReport">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"partyID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"shopID">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Идентификатор магазина">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"fromTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Начало временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"toTime">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Конец временного отрезка">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>
        }, #{
          <<"name">> => <<"reportType">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Тип отчета">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"enum">> => [ <<"paymentRegistry">> ]
        } ],
        <<"responses">> => #{
          <<"201">> => #{
            <<"description">> => <<"Отчет создан">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Report">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Ошибочные данные для генерации">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/inline_response_400_1">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          }
        }
      }
    },
    <<"/reports/{reportID}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Reports">> ],
        <<"description">> => <<"Получить отчет по данному идентификатору">>,
        <<"operationId">> => <<"getReport">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"reportID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор отчета">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Найденный отчет">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/Report">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/reports/{reportID}/cancel">> => #{
      <<"post">> => #{
        <<"tags">> => [ <<"Reports">> ],
        <<"description">> => <<"Отменить указанный отчет. Отчеты типа `provisionOfService` отменить нельзя">>,
        <<"operationId">> => <<"cancelReport">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"reportID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор отчета">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        } ],
        <<"responses">> => #{
          <<"202">> => #{
            <<"description">> => <<"Запрос на отмену отчета принят">>
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    },
    <<"/reports/{reportID}/files/{fileID}/download">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"Reports">> ],
        <<"description">> => <<"Скачать файл">>,
        <<"operationId">> => <<"downloadFile">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 32,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        }, #{
          <<"name">> => <<"reportID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор отчета">>,
          <<"required">> => true,
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        }, #{
          <<"name">> => <<"fileID">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор файла">>,
          <<"required">> => true,
          <<"type">> => <<"string">>,
          <<"maxLength">> => 40,
          <<"minLength">> => 1
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Download link">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/ReportLink">>
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Неверные данные">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/DefaultLogicError">>
            }
          },
          <<"401">> => #{
            <<"description">> => <<"Ошибка авторизации">>
          },
          <<"404">> => #{
            <<"description">> => <<"Заданный ресурс не найден">>,
            <<"schema">> => #{
              <<"$ref">> => <<"#/definitions/GeneralError">>
            }
          }
        }
      }
    }
  },
  <<"securityDefinitions">> => #{
    <<"bearer">> => #{
      <<"description">> => <<"Для аутентификации вызовов мы используем [JWT](https://jwt.io).\nCоответствующий ключ передается в заголовке.\n\n```shell\n Authorization: Bearer {JWT}\n```\n\nПосмотреть ваш API-ключ вы можете в личном кабинете.\n\nПомните, что вы никому не должны передавать ваш API ключ!\n">>,
      <<"type">> => <<"apiKey">>,
      <<"name">> => <<"Authorization">>,
      <<"in">> => <<"header">>
    }
  },
  <<"definitions">> => #{
    <<"AmountResult">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"amount">>, <<"currency">> ],
        <<"properties">> => #{
          <<"amount">> => #{
            <<"type">> => <<"integer">>,
            <<"format">> => <<"int64">>,
            <<"description">> => <<"Стоимость предлагаемых товаров или услуг, в минорных денежных\nединицах, например в копейках в случае указания российских рублей в\nкачестве валюты.\n">>
          },
          <<"currency">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Валюта, символьный код согласно [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          }
        }
      }, #{ } ]
    },
    <<"BankAccount">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"account">>, <<"bankBik">>, <<"bankName">>, <<"bankPostAccount">> ],
      <<"properties">> => #{
        <<"account">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Номер счёта">>,
          <<"pattern">> => <<"^\\d{20}$">>
        },
        <<"bankName">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Наименование юридического лица банковской организации">>,
          <<"maxLength">> => 100
        },
        <<"bankPostAccount">> => #{
          <<"type">> => <<"string">>,
          <<"pattern">> => <<"^\\d{20}$">>
        },
        <<"bankBik">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"БИК банковской организации">>,
          <<"pattern">> => <<"^\\d{9}$">>
        }
      },
      <<"description">> => <<"Данные расчётного счёта в банковской организации, ведущей деятельность под\nюрисдикцией РФ.\n">>
    },
    <<"BankCardDetails">> => #{
      <<"required">> => [ <<"cardNumberMask">>, <<"paymentSystem">> ],
      <<"properties">> => #{
        <<"cardNumberMask">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Маскированый номер карты">>,
          <<"pattern">> => <<"^\\d{6,8}\\*+\\d{2,4}$">>
        },
        <<"bin">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"BIN банка-эмитента карты">>,
          <<"pattern">> => <<"^(\\d{0}|\\d{6,8})$">>
        },
        <<"lastDigits">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Последние цифры номера карты">>,
          <<"pattern">> => <<"^\\d{2,4}$">>
        },
        <<"paymentSystem">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Платежная система">>,
          <<"enum">> => [ <<"visa">>, <<"mastercard">>, <<"visaelectron">>, <<"maestro">>, <<"forbrugsforeningen">>, <<"dankort">>, <<"amex">>, <<"dinersclub">>, <<"discover">>, <<"unionpay">>, <<"jcb">>, <<"nspkmir">>, <<"elo">>, <<"rupay">>, <<"dummy">>, <<"uzcard">> ]
        },
        <<"tokenProvider">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Провайдер платежных токенов">>,
          <<"enum">> => [ <<"applepay">>, <<"googlepay">>, <<"samsungpay">>, <<"yandexpay">> ]
        }
      }
    },
    <<"BankCardPaymentSystem">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Платежная система">>,
      <<"enum">> => [ <<"visa">>, <<"mastercard">>, <<"visaelectron">>, <<"maestro">>, <<"forbrugsforeningen">>, <<"dankort">>, <<"amex">>, <<"dinersclub">>, <<"discover">>, <<"unionpay">>, <<"jcb">>, <<"nspkmir">>, <<"elo">>, <<"rupay">>, <<"dummy">>, <<"uzcard">> ]
    },
    <<"BankCardTokenProvider">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Провайдер платежных токенов">>,
      <<"enum">> => [ <<"applepay">>, <<"googlepay">>, <<"samsungpay">>, <<"yandexpay">> ]
    },
    <<"Chargeback">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"bodyAmount">>, <<"bodyCurrency">>, <<"chargebackId">>, <<"createdAt">>, <<"invoiceId">>, <<"levyAmount">>, <<"levyCurrency">>, <<"paymentId">>, <<"shopID">> ],
      <<"properties">> => #{
        <<"invoiceId">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор инвойса">>
        },
        <<"paymentId">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор платежа">>
        },
        <<"chargebackId">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор чарджбэка">>
        },
        <<"externalId">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Внешний идентификатор">>
        },
        <<"shopID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор магазина">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время создания">>
        },
        <<"levyAmount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Сумма списываемых средств у чарджбека">>,
          <<"minimum">> => 1
        },
        <<"levyCurrency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"bodyAmount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Сумма чарджбэка">>,
          <<"minimum">> => 1
        },
        <<"bodyCurrency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"fee">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Комиссия системы, в минорных денежных единицах">>,
          <<"minimum">> => 0
        },
        <<"providerFee">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Комиссия провайдера, в минорных денежных единицах">>,
          <<"minimum">> => 0
        },
        <<"externalFee">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Комиссия внешней системы системы, в минорных денежных единицах">>,
          <<"minimum">> => 0
        },
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Статус чарджбэка">>,
          <<"enum">> => [ <<"pending">>, <<"accepted">>, <<"rejected">>, <<"cancelled">> ]
        },
        <<"stage">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Этап прохождения чарджбэка">>,
          <<"enum">> => [ <<"chargeback">>, <<"pre_arbitration">>, <<"arbitration">> ]
        },
        <<"chargebackReason">> => #{
          <<"$ref">> => <<"#/definitions/ChargebackReason">>
        },
        <<"content">> => #{
          <<"$ref">> => <<"#/definitions/Content">>
        }
      },
      <<"example">> => #{
        <<"chargebackId">> => <<"chargebackId">>,
        <<"levyAmount">> => 1,
        <<"providerFee">> => 0,
        <<"fee">> => 0,
        <<"levyCurrency">> => <<"levyCurrency">>,
        <<"externalId">> => <<"externalId">>,
        <<"bodyAmount">> => 1,
        <<"bodyCurrency">> => <<"bodyCurrency">>,
        <<"content">> => #{
          <<"data">> => <<"data">>,
          <<"type">> => <<"type">>
        },
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"stage">> => <<"chargeback">>,
        <<"paymentId">> => <<"paymentId">>,
        <<"chargebackReason">> => #{
          <<"code">> => <<"code">>,
          <<"category">> => #{ }
        },
        <<"invoiceId">> => <<"invoiceId">>,
        <<"shopID">> => <<"shopID">>,
        <<"externalFee">> => 0,
        <<"status">> => <<"pending">>
      }
    },
    <<"ChargebackCategory">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Категория чарджбэка">>,
      <<"enum">> => [ <<"fraud">>, <<"dispute">>, <<"authorisation">>, <<"processing_error">> ]
    },
    <<"ChargebackReason">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"category">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Код категории">>
        },
        <<"category">> => #{
          <<"$ref">> => <<"#/definitions/ChargebackCategory">>
        }
      },
      <<"description">> => <<"Данные о причине чарджбэка">>,
      <<"example">> => #{
        <<"code">> => <<"code">>,
        <<"category">> => #{ }
      }
    },
    <<"ChargebackStage">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Этап прохождения чарджбэка">>,
      <<"enum">> => [ <<"chargeback">>, <<"pre_arbitration">>, <<"arbitration">> ]
    },
    <<"ChargebackStatus">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Статус чарджбэка">>,
      <<"enum">> => [ <<"pending">>, <<"accepted">>, <<"rejected">>, <<"cancelled">> ]
    },
    <<"ClientInfo">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"fingerprint">> ],
      <<"properties">> => #{
        <<"fingerprint">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Уникальный отпечаток user agent'а плательщика">>,
          <<"maxLength">> => 1000
        },
        <<"ip">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"ip-address">>,
          <<"description">> => <<"IP-адрес плательщика">>,
          <<"maxLength">> => 45
        }
      },
      <<"description">> => <<"Данные клиентского устройства плательщика">>
    },
    <<"ContactInfo">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"email">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"email">>,
          <<"description">> => <<"Адрес электронной почты">>,
          <<"maxLength">> => 100
        },
        <<"phoneNumber">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"^\\+\\d{4,15}$">>,
          <<"description">> => <<"Номер мобильного телефона с международным префиксом согласно\n[E.164](https://en.wikipedia.org/wiki/E.164).\n">>
        }
      },
      <<"description">> => <<"Контактные данные">>
    },
    <<"Content">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип данных">>
        },
        <<"data">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"byte">>,
          <<"description">> => <<"Данные">>,
          <<"pattern">> => <<"^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$">>
        }
      },
      <<"example">> => #{
        <<"data">> => <<"data">>,
        <<"type">> => <<"type">>
      }
    },
    <<"ContinuationToken">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>
    },
    <<"CountResult">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"count">>, <<"currency">> ],
        <<"properties">> => #{
          <<"count">> => #{
            <<"type">> => <<"integer">>,
            <<"format">> => <<"int64">>,
            <<"description">> => <<"Общее количество операций">>,
            <<"minimum">> => 0
          },
          <<"currency">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Валюта, символьный код согласно [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          }
        }
      }, #{ } ]
    },
    <<"CryptoCurrency">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Тип криптовалюты">>,
      <<"enum">> => [ <<"bitcoin">>, <<"litecoin">>, <<"bitcoinCash">>, <<"ripple">>, <<"ethereum">>, <<"zcash">> ]
    },
    <<"CryptoWalletDetails">> => #{
      <<"required">> => [ <<"cryptoCurrency">> ],
      <<"properties">> => #{
        <<"cryptoCurrency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип криптовалюты">>,
          <<"enum">> => [ <<"bitcoin">>, <<"litecoin">>, <<"bitcoinCash">>, <<"ripple">>, <<"ethereum">>, <<"zcash">> ]
        }
      }
    },
    <<"Currency">> => #{
      <<"type">> => <<"string">>,
      <<"pattern">> => <<"^[A-Z]{3}$">>,
      <<"description">> => <<"Валюта, символьный код согласно [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).">>
    },
    <<"CustomerPayer">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/Payer">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"customerID">> ],
        <<"properties">> => #{
          <<"customerID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор плательщика">>,
            <<"minLength">> => 1,
            <<"maxLength">> => 40
          },
          <<"paymentToolToken">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Токен платежного средства, предоставленного плательщиком">>,
            <<"maxLength">> => 1000
          },
          <<"paymentToolDetails">> => #{
            <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
          }
        }
      } ],
      <<"description">> => <<"Многоразовое платежное средство">>
    },
    <<"DefaultLogicError">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Код ошибки](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"invalidRequest">>, <<"ambiguousPartyID">>, <<"invalidDeadline">>, <<"invalidPartyID">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Человекочитаемое описание ошибки">>
        }
      }
    },
    <<"DigitalWalletDetails">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"provider">> ],
      <<"properties">> => #{
        <<"provider">> => #{
          <<"x-rebillyMerge">> => [ #{
            <<"$ref">> => <<"#/definitions/DigitalWalletProvider">>
          } ]
        }
      }
    },
    <<"DigitalWalletProvider">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Провайдер электронных денежных средств.\n\nДополнительные детали провайдера можно узнать, вызвав [справочную операцию](#operation/getServiceProviderByID).\n">>,
      <<"example">> => <<"qiwi">>
    },
    <<"ExternalID">> => #{
      <<"type">> => <<"string">>,
      <<"minLength">> => 1,
      <<"maxLength">> => 40,
      <<"description">> => <<"Уникальный в рамках платформы идентификатор сущности для данного участника.\nИспользуется для обеспечения идемпотентности запроса.\n">>
    },
    <<"FileMeta">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"filename">>, <<"id">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор файла">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"filename">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Имя файла">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 1000
        },
        <<"signatures">> => #{
          <<"$ref">> => <<"#/definitions/FileMeta_signatures">>
        }
      },
      <<"example">> => #{
        <<"filename">> => <<"filename">>,
        <<"id">> => <<"id">>,
        <<"signatures">> => #{
          <<"sha256">> => <<"sha256">>,
          <<"md5">> => <<"md5">>
        }
      }
    },
    <<"GeneralError">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"message">> ],
      <<"properties">> => #{
        <<"message">> => #{
          <<"type">> => <<"string">>
        }
      }
    },
    <<"GeoLocationInfo">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"cityGeoID">>, <<"countryGeoID">> ],
      <<"properties">> => #{
        <<"cityGeoID">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>
        },
        <<"countryGeoID">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>
        }
      },
      <<"description">> => <<"Информация о геопозиции">>
    },
    <<"GroupByShopAmountResult">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"groupBySHopResults">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/ShopAmountResult">>
          }
        }
      },
      <<"description">> => <<"Сгруппированый по магазинам список текущих балансов\n">>,
      <<"example">> => #{
        <<"groupBySHopResults">> => [ #{
          <<"amountResults">> => [ <<"">>, <<"">> ],
          <<"id">> => <<"id">>
        }, #{
          <<"amountResults">> => [ <<"">>, <<"">> ],
          <<"id">> => <<"id">>
        } ]
      }
    },
    <<"InternationalBankAccount">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"number">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"123006951">>,
          <<"description">> => <<"Номер счёта\n">>,
          <<"pattern">> => <<"^[0-9A-Z]{8,40}$">>
        },
        <<"iban">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"GR1601101250000000012300695">>,
          <<"description">> => <<"International Bank Account Number [ISO 13616](https://en.wikipedia.org/wiki/International_Bank_Account_Number)\n">>,
          <<"pattern">> => <<"^[A-Z0-9]{3,35}$">>
        },
        <<"bankDetails">> => #{
          <<"$ref">> => <<"#/definitions/InternationalBankDetails">>
        },
        <<"correspondentBankAccount">> => #{
          <<"$ref">> => <<"#/definitions/InternationalCorrespondentBankAccount">>
        }
      },
      <<"description">> => <<"Данные международного банковского счёта">>
    },
    <<"InternationalBankDetails">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"bic">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RZBAATWW\n">>,
          <<"description">> => <<"Business Identifier Code [ISO 9362](https://en.wikipedia.org/wiki/ISO_9362).\n">>,
          <<"pattern">> => <<"^([A-Z0-9]{8}|[A-Z0-9]{11})$">>
        },
        <<"abartn">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"129131673">>,
          <<"description">> => <<"[ABA Routing Transit Number](https://en.wikipedia.org/wiki/ABA_routing_transit_number)\nбанковской организации, специфичный для банковской системы USA.\n">>,
          <<"pattern">> => <<"^[0-9]{9}$">>
        },
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RAIFFEISEN BANK INTERNATIONAL AG\n">>,
          <<"description">> => <<"Наименование юридического лица банковской организации">>,
          <<"maxLength">> => 100
        },
        <<"countryCode">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"RUS">>,
          <<"description">> => <<"Страна резиденции банковской организации,\nalpha-3 код по стандарту [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)\n">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"address">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"1030, VIENNA, AM STADTPARK 9\n">>,
          <<"description">> => <<"Адрес юридического лица банковской организации">>,
          <<"maxLength">> => 1000
        }
      },
      <<"description">> => <<"Данные международной банковской организации">>
    },
    <<"InternationalCorrespondentBankAccount">> => #{
      <<"allOf">> => [ #{
        <<"description">> => <<"Данные корреспондентского счёта указанного банка">>
      }, #{
        <<"$ref">> => <<"#/definitions/InternationalBankAccount">>
      } ]
    },
    <<"Invoice">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"amount">>, <<"createdAt">>, <<"currency">>, <<"dueDate">>, <<"id">>, <<"metadata">>, <<"product">>, <<"shopID">> ],
        <<"properties">> => #{
          <<"id">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор инвойса">>
          },
          <<"externalID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Внешний идентификатор">>
          },
          <<"shopID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор магазина">>
          },
          <<"createdAt">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Дата и время создания">>
          },
          <<"dueDate">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Дата и время окончания действия">>
          },
          <<"amount">> => #{
            <<"type">> => <<"integer">>,
            <<"format">> => <<"int64">>,
            <<"description">> => <<"Стоимость предлагаемых товаров или услуг, в минорных денежных единицах,\nнапример в копейках в случае указания российских рублей в качестве валюты.\n">>,
            <<"minimum">> => 1
          },
          <<"currency">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Валюта, символьный код согласно [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          },
          <<"product">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Наименование предлагаемых товаров или услуг">>,
            <<"maxLength">> => 100
          },
          <<"description">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Описание предлагаемых товаров или услуг">>,
            <<"maxLength">> => 1000
          },
          <<"invoiceTemplateID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор шаблона (для инвойсов, созданных по шаблону).">>
          },
          <<"cart">> => #{
            <<"$ref">> => <<"#/definitions/InvoiceCart">>
          },
          <<"metadata">> => #{
            <<"type">> => <<"object">>,
            <<"description">> => <<"Связанные с инвойсом метаданные">>,
            <<"properties">> => #{ }
          }
        }
      }, #{
        <<"$ref">> => <<"#/definitions/InvoiceStatus">>
      } ]
    },
    <<"InvoiceCart">> => #{
      <<"type">> => <<"array">>,
      <<"description">> => <<"Корзина с набором позиций продаваемых товаров или услуг\n">>,
      <<"items">> => #{
        <<"$ref">> => <<"#/definitions/InvoiceLine">>
      },
      <<"minItems">> => 1,
      <<"maxItems">> => 100
    },
    <<"InvoiceLine">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"price">>, <<"product">>, <<"quantity">> ],
      <<"properties">> => #{
        <<"product">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Описание предлагаемого товара или услуги">>,
          <<"maxLength">> => 1000
        },
        <<"quantity">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Количество единиц товаров или услуг, предлагаемых на продажу в этой\nпозиции\n">>,
          <<"minimum">> => 1,
          <<"default">> => 1
        },
        <<"price">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Цена предлагаемого товара или услуги, в минорных денежных единицах, например\nв копейках в случае указания российских рублей в качестве валюты\n">>,
          <<"minimum">> => 1
        },
        <<"cost">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Суммарная стоимость позиции с учётом количества единиц товаров или услуг\n">>,
          <<"minimum">> => 1
        },
        <<"taxMode">> => #{
          <<"$ref">> => <<"#/definitions/InvoiceLineTaxMode">>
        }
      },
      <<"description">> => <<"Позиция товара или услуги">>
    },
    <<"InvoiceLineTaxMode">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип схемы налогообложения">>,
          <<"enum">> => [ <<"InvoiceLineTaxVAT">> ]
        }
      },
      <<"description">> => <<"Схема налогообложения предлагаемого товара или услуги.\n\nУказывается, только если предлагаемый товар или услуга облагается налогом.\n">>,
      <<"x-discriminator-is-enum">> => true
    },
    <<"InvoiceLineTaxVAT">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/InvoiceLineTaxMode">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"rate">> ],
        <<"properties">> => #{
          <<"rate">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Ставка налога">>,
            <<"enum">> => [ <<"0%">>, <<"10%">>, <<"18%">>, <<"20%">>, <<"10/110">>, <<"18/118">>, <<"20/120">> ]
          }
        },
        <<"description">> => <<"Налог на добавленную стоимость в юрисдикции РФ">>
      } ]
    },
    <<"InvoiceStatus">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"status">> ],
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Статус инвойса">>,
          <<"enum">> => [ <<"unpaid">>, <<"cancelled">>, <<"paid">>, <<"fulfilled">> ]
        },
        <<"reason">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Причина отмены или погашения инвойса">>,
          <<"maxLength">> => 1000
        }
      }
    },
    <<"MobileCommerceDetails">> => #{
      <<"required">> => [ <<"phoneNumber">> ],
      <<"properties">> => #{
        <<"phoneNumber">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"+7******0102">>,
          <<"description">> => <<"Маскированый номер мобильного телефона">>,
          <<"pattern">> => <<"^\\+\\d\\*{1,10}\\d{2,4}$">>
        }
      }
    },
    <<"OffsetAmount">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"offset">> ],
      <<"properties">> => #{
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Стоимость предлагаемых товаров или услуг, в минорных денежных\nединицах, например в копейках в случае указания российских рублей в\nкачестве валюты.\n">>,
          <<"minimum">> => 1
        },
        <<"offset">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Номер интервала в списке">>,
          <<"minimum">> => 0
        }
      },
      <<"example">> => #{
        <<"amount">> => 1,
        <<"offset">> => 0
      }
    },
    <<"OffsetCount">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"count">>, <<"offset">> ],
        <<"properties">> => #{
          <<"count">> => #{
            <<"type">> => <<"integer">>,
            <<"format">> => <<"int64">>,
            <<"description">> => <<"Общее количество операций">>,
            <<"minimum">> => 0
          },
          <<"offset">> => #{
            <<"type">> => <<"integer">>,
            <<"format">> => <<"int64">>,
            <<"description">> => <<"Номер интервала в списке">>,
            <<"minimum">> => 0
          }
        }
      }, #{ } ]
    },
    <<"Payer">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"payerType">> ],
      <<"discriminator">> => <<"payerType">>,
      <<"properties">> => #{
        <<"payerType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип платежного средства">>,
          <<"enum">> => [ <<"CustomerPayer">>, <<"PaymentResourcePayer">>, <<"RecurrentPayer">> ]
        }
      },
      <<"x-discriminator-is-enum">> => true
    },
    <<"PaymentError">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Основной код ошибки">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubError">>
        }
      },
      <<"description">> => <<"[Ошибка, возникшая в процессе проведения платежа](#tag/Error-Codes)\n">>
    },
    <<"PaymentFlow">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"type">> ],
      <<"discriminator">> => <<"type">>,
      <<"properties">> => #{
        <<"type">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип процесса выполнения платежа">>,
          <<"enum">> => [ <<"PaymentFlowInstant">>, <<"PaymentFlowHold">> ]
        }
      },
      <<"x-discriminator-is-enum">> => true
    },
    <<"PaymentFlowHold">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentFlow">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"onHoldExpiration">> ],
        <<"properties">> => #{
          <<"onHoldExpiration">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Политика управления удержанием денежных средств">>,
            <<"default">> => <<"cancel">>,
            <<"enum">> => [ <<"cancel">>, <<"capture">> ]
          },
          <<"heldUntil">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Дата и время, до которого происходит удержание денежных средств">>
          }
        }
      } ]
    },
    <<"PaymentFlowInstant">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentFlow">>
      }, #{ } ]
    },
    <<"PaymentInstitutionAccount">> => #{
      <<"type">> => <<"object">>,
      <<"description">> => <<"Аккаунт платёжной организации">>
    },
    <<"PaymentMakeRecurrent">> => #{
      <<"type">> => <<"boolean">>,
      <<"description">> => <<"Признак создания родительского рекуррентного платежа.\nУспешно проведеный платеж с этим признаком можно использовать как родительский в других рекуррентных платежах.\n">>,
      <<"default">> => <<"false">>
    },
    <<"PaymentRecurrentParent">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"invoiceID">>, <<"paymentID">> ],
      <<"properties">> => #{
        <<"invoiceID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор инвойса">>
        },
        <<"paymentID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор платежа">>
        }
      },
      <<"description">> => <<"Родительский платеж, на основе которого создан текущий рекуррентный платеж">>
    },
    <<"PaymentResource">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"paymentToolToken">> ],
      <<"properties">> => #{
        <<"paymentToolToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Токен платежного средства, предоставленного плательщиком">>,
          <<"maxLength">> => 1000
        },
        <<"paymentSession">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор платежной сессии">>,
          <<"maxLength">> => 1000
        },
        <<"paymentToolDetails">> => #{
          <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
        },
        <<"clientInfo">> => #{
          <<"$ref">> => <<"#/definitions/PaymentResource_clientInfo">>
        }
      },
      <<"description">> => <<"Данные одноразового платежного средства">>
    },
    <<"PaymentResourcePayer">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/Payer">>
      }, #{
        <<"$ref">> => <<"#/definitions/PaymentResource">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"contactInfo">> ],
        <<"properties">> => #{
          <<"contactInfo">> => #{
            <<"$ref">> => <<"#/definitions/ContactInfo">>
          }
        }
      } ],
      <<"description">> => <<"Одноразовое платежное средство">>
    },
    <<"PaymentSearchResult">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentStatus">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"amount">>, <<"createdAt">>, <<"currency">>, <<"flow">>, <<"id">>, <<"invoiceID">>, <<"payer">> ],
        <<"properties">> => #{
          <<"id">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор платежа">>
          },
          <<"shortID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Cокращенный идентификатор платежа и инвойса (spid)">>
          },
          <<"invoiceID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор инвойса, в рамках которого был создан платеж">>
          },
          <<"externalID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Внешний идентификатор">>
          },
          <<"shopID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор магазина, в рамках которого был создан платеж">>
          },
          <<"createdAt">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Дата и время создания">>
          },
          <<"amount">> => #{
            <<"type">> => <<"integer">>,
            <<"format">> => <<"int64">>,
            <<"description">> => <<"Стоимость предлагаемых товаров или услуг, в минорных денежных\nединицах, например в копейках в случае указания российских рублей в\nкачестве валюты.\n">>,
            <<"minimum">> => 1
          },
          <<"fee">> => #{
            <<"type">> => <<"integer">>,
            <<"format">> => <<"int64">>,
            <<"description">> => <<"Комиссия системы, в минорных денежных единицах">>,
            <<"minimum">> => 0
          },
          <<"currency">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Валюта, символьный код согласно [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          },
          <<"payer">> => #{
            <<"$ref">> => <<"#/definitions/Payer">>
          },
          <<"flow">> => #{
            <<"$ref">> => <<"#/definitions/PaymentFlow">>
          },
          <<"geoLocationInfo">> => #{
            <<"$ref">> => <<"#/definitions/GeoLocationInfo">>
          },
          <<"metadata">> => #{
            <<"type">> => <<"object">>,
            <<"description">> => <<"Связанные с платежом метаданные">>,
            <<"properties">> => #{ }
          },
          <<"statusChangedAt">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Дата и время изменения статуса платежа">>
          },
          <<"transactionInfo">> => #{
            <<"$ref">> => <<"#/definitions/TransactionInfo">>
          },
          <<"makeRecurrent">> => #{
            <<"$ref">> => <<"#/definitions/PaymentMakeRecurrent">>
          }
        }
      } ]
    },
    <<"PaymentsErrorsDistributionResult">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"error">>, <<"percents">> ],
        <<"properties">> => #{
          <<"error">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Тип ошибки">>
          },
          <<"percents">> => #{
            <<"type">> => <<"number">>,
            <<"format">> => <<"double">>,
            <<"description">> => <<"Относительное колличество ошибок в процентах">>,
            <<"minimum">> => 0
          }
        }
      }, #{ } ]
    },
    <<"PaymentsSubErrorsDistributionResult">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"error">>, <<"percents">> ],
        <<"properties">> => #{
          <<"error">> => #{
            <<"$ref">> => <<"#/definitions/SubError">>
          },
          <<"percents">> => #{
            <<"type">> => <<"number">>,
            <<"format">> => <<"double">>,
            <<"description">> => <<"Относительное колличество ошибок в процентах">>,
            <<"minimum">> => 0
          }
        }
      }, #{ } ]
    },
    <<"PaymentStatus">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"status">> ],
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Статус платежа">>,
          <<"enum">> => [ <<"pending">>, <<"processed">>, <<"captured">>, <<"cancelled">>, <<"refunded">>, <<"failed">> ]
        },
        <<"error">> => #{
          <<"$ref">> => <<"#/definitions/PaymentError">>
        }
      }
    },
    <<"PaymentsToolDistributionResult">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"name">>, <<"percents">> ],
        <<"properties">> => #{
          <<"name">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Название платежного средства">>
          },
          <<"percents">> => #{
            <<"type">> => <<"number">>,
            <<"format">> => <<"double">>,
            <<"description">> => <<"Колличество использования платежного инструмента в процентах">>,
            <<"minimum">> => 0
          }
        }
      }, #{ } ]
    },
    <<"PaymentTerminalDetails">> => #{
      <<"required">> => [ <<"provider">> ],
      <<"properties">> => #{
        <<"provider">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Провайдер терминальной сети">>,
          <<"enum">> => [ <<"euroset">>, <<"wechat">>, <<"alipay">>, <<"zotapay">>, <<"qps">>, <<"uzcard">>, <<"rbs">> ]
        }
      }
    },
    <<"PaymentTerminalProvider">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Провайдер терминальной сети">>,
      <<"enum">> => [ <<"euroset">>, <<"wechat">>, <<"alipay">>, <<"zotapay">>, <<"qps">>, <<"uzcard">>, <<"rbs">> ]
    },
    <<"PaymentToolDetails">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"detailsType">> ],
      <<"discriminator">> => <<"detailsType">>,
      <<"properties">> => #{
        <<"detailsType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип информации о платежном средстве">>
        }
      },
      <<"description">> => <<"Детали платежного средства">>
    },
    <<"PaymentToolDetailsBankCard">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
      }, #{
        <<"$ref">> => <<"#/definitions/BankCardDetails">>
      } ]
    },
    <<"PaymentToolDetailsCryptoWallet">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
      }, #{
        <<"$ref">> => <<"#/definitions/CryptoWalletDetails">>
      } ]
    },
    <<"PaymentToolDetailsDigitalWallet">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
      }, #{
        <<"$ref">> => <<"#/definitions/DigitalWalletDetails">>
      } ]
    },
    <<"PaymentToolDetailsMobileCommerce">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
      }, #{
        <<"$ref">> => <<"#/definitions/MobileCommerceDetails">>
      } ]
    },
    <<"PaymentToolDetailsPaymentTerminal">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
      }, #{
        <<"$ref">> => <<"#/definitions/PaymentTerminalDetails">>
      } ]
    },
    <<"Payout">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"amount">>, <<"createdAt">>, <<"currency">>, <<"id">>, <<"payoutToolDetails">>, <<"shopID">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор выплаты">>
        },
        <<"shopID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор магазина">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время создания">>
        },
        <<"cancellationDetails">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Детали отмены выплаты">>,
          <<"maxLength">> => 1000
        },
        <<"amount">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Сумма выплаты в минорных денежных\nединицах, например в копейках в случае указания российских рублей в\nкачестве валюты.\n">>,
          <<"minimum">> => 1
        },
        <<"fee">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Комиссия системы, в минорных денежных единицах">>,
          <<"minimum">> => 0
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"payoutToolDetails">> => #{
          <<"$ref">> => <<"#/definitions/PayoutToolDetails">>
        },
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Статус выплаты">>
        },
        <<"metadata">> => #{
          <<"type">> => <<"object">>,
          <<"example">> => #{
            <<"payoutDesc">> => <<"Custom payout">>
          },
          <<"description">> => <<"Произвольный, специфичный для клиента API и непрозрачный для системы набор данных,\nассоциированных с данной выплатой\n">>,
          <<"properties">> => #{ }
        }
      },
      <<"example">> => #{
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"amount">> => 1,
        <<"payoutToolDetails">> => #{
          <<"detailsType">> => <<"detailsType">>
        },
        <<"metadata">> => #{
          <<"payoutDesc">> => <<"Custom payout">>
        },
        <<"cancellationDetails">> => <<"cancellationDetails">>,
        <<"fee">> => 0,
        <<"currency">> => <<"currency">>,
        <<"id">> => <<"id">>,
        <<"shopID">> => <<"shopID">>,
        <<"status">> => <<"status">>
      }
    },
    <<"PayoutToolDetails">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"detailsType">> ],
      <<"discriminator">> => <<"detailsType">>,
      <<"properties">> => #{
        <<"detailsType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип средства вывода">>
        }
      },
      <<"description">> => <<"Данные средства вывода">>,
      <<"example">> => #{
        <<"detailsType">> => <<"detailsType">>
      }
    },
    <<"PayoutToolDetailsBankAccount">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PayoutToolDetails">>
      }, #{
        <<"$ref">> => <<"#/definitions/BankAccount">>
      } ]
    },
    <<"PayoutToolDetailsInternationalBankAccount">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PayoutToolDetails">>
      }, #{
        <<"$ref">> => <<"#/definitions/InternationalBankAccount">>
      } ]
    },
    <<"PayoutToolDetailsPaymentInstitutionAccount">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PayoutToolDetails">>
      }, #{
        <<"$ref">> => <<"#/definitions/PaymentInstitutionAccount">>
      } ]
    },
    <<"PayoutToolDetailsWalletInfo">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/PayoutToolDetails">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"walletID">> ],
        <<"properties">> => #{
          <<"walletID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор кошелька">>,
            <<"minLength">> => 1,
            <<"maxLength">> => 40
          }
        }
      } ]
    },
    <<"RecurrentPayer">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/Payer">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"contactInfo">>, <<"recurrentParentPayment">> ],
        <<"properties">> => #{
          <<"contactInfo">> => #{
            <<"$ref">> => <<"#/definitions/ContactInfo">>
          },
          <<"recurrentParentPayment">> => #{
            <<"$ref">> => <<"#/definitions/PaymentRecurrentParent">>
          },
          <<"paymentToolToken">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Токен платежного средства, предоставленного плательщиком">>,
            <<"maxLength">> => 1000
          },
          <<"paymentToolDetails">> => #{
            <<"$ref">> => <<"#/definitions/PaymentToolDetails">>
          }
        }
      } ],
      <<"description">> => <<"Многоразовое платежное средство на основе другого платежа">>
    },
    <<"Refund">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"amount">>, <<"createdAt">>, <<"currency">>, <<"id">>, <<"shopID">> ],
        <<"properties">> => #{
          <<"id">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор возврата">>
          },
          <<"externalID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Внешний идентификатор">>
          },
          <<"createdAt">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"date-time">>,
            <<"description">> => <<"Дата и время осуществления">>
          },
          <<"amount">> => #{
            <<"type">> => <<"integer">>,
            <<"format">> => <<"int64">>,
            <<"description">> => <<"Сумма возврата, в минорных денежных единицах, например в копейках в случае указания российских рублей в качестве валюты.\n">>,
            <<"minimum">> => 1
          },
          <<"currency">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Валюта, символьный код согласно [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).">>,
            <<"pattern">> => <<"^[A-Z]{3}$">>
          },
          <<"reason">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Причина осуществления возврата">>
          },
          <<"shopID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор магазина">>
          }
        }
      }, #{
        <<"$ref">> => <<"#/definitions/RefundStatus">>
      } ]
    },
    <<"RefundSearchResult">> => #{
      <<"allOf">> => [ #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"invoiceID">>, <<"paymentID">> ],
        <<"properties">> => #{
          <<"invoiceID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор инвойса">>
          },
          <<"paymentID">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор платежа">>
          }
        }
      }, #{
        <<"$ref">> => <<"#/definitions/Refund">>
      } ]
    },
    <<"RefundStatus">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"status">> ],
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Статус возврата">>,
          <<"enum">> => [ <<"pending">>, <<"succeeded">>, <<"failed">> ]
        },
        <<"error">> => #{
          <<"$ref">> => <<"#/definitions/RefundStatus_error">>
        }
      }
    },
    <<"Report">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"createdAt">>, <<"files">>, <<"fromTime">>, <<"id">>, <<"partyID">>, <<"reportType">>, <<"status">>, <<"toTime">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>,
          <<"description">> => <<"Идентификатор отчета">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время создания">>
        },
        <<"fromTime">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время начала периода">>
        },
        <<"toTime">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время конца периода">>
        },
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Статус формирования отчета">>,
          <<"enum">> => [ <<"pending">>, <<"created">> ]
        },
        <<"reportType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип отчета">>,
          <<"enum">> => [ <<"provisionOfService">>, <<"paymentRegistry">>, <<"paymentRegistryByPayout">> ]
        },
        <<"partyID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор участника">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"shopID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор магазина">>,
          <<"minLength">> => 1,
          <<"maxLength">> => 40
        },
        <<"files">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/FileMeta">>
          }
        }
      },
      <<"example">> => #{
        <<"reportType">> => <<"provisionOfService">>,
        <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"fromTime">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"files">> => [ #{
          <<"filename">> => <<"filename">>,
          <<"id">> => <<"id">>,
          <<"signatures">> => #{
            <<"sha256">> => <<"sha256">>,
            <<"md5">> => <<"md5">>
          }
        }, #{
          <<"filename">> => <<"filename">>,
          <<"id">> => <<"id">>,
          <<"signatures">> => #{
            <<"sha256">> => <<"sha256">>,
            <<"md5">> => <<"md5">>
          }
        } ],
        <<"id">> => 0,
        <<"shopID">> => <<"shopID">>,
        <<"partyID">> => <<"partyID">>,
        <<"toTime">> => <<"2000-01-23T04:56:07.000+00:00">>,
        <<"status">> => <<"pending">>
      }
    },
    <<"ReportLink">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"url">> ],
      <<"properties">> => #{
        <<"url">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"URL файла">>
        }
      },
      <<"example">> => #{
        <<"url">> => <<"url">>
      }
    },
    <<"Residence">> => #{
      <<"type">> => <<"string">>,
      <<"pattern">> => <<"^[A-Z]{3}$">>,
      <<"description">> => <<"Резиденция, alpha-3 код по стандарту [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)">>,
      <<"example">> => <<"RUS">>
    },
    <<"Shop">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"categoryID">>, <<"contractID">>, <<"createdAt">>, <<"details">>, <<"id">>, <<"isBlocked">>, <<"isSuspended">>, <<"location">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор магазина">>
        },
        <<"createdAt">> => #{
          <<"type">> => <<"string">>,
          <<"format">> => <<"date-time">>,
          <<"description">> => <<"Дата и время создания">>
        },
        <<"isBlocked">> => #{
          <<"type">> => <<"boolean">>,
          <<"description">> => <<"Заблокирован ли магазин?">>
        },
        <<"isSuspended">> => #{
          <<"type">> => <<"boolean">>,
          <<"description">> => <<"Приостановлены ли операции в рамках магазина?">>
        },
        <<"categoryID">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>,
          <<"description">> => <<"Идентификатор категории товаров и услуг, предлагаемых в этом магазине\n">>
        },
        <<"location">> => #{
          <<"$ref">> => <<"#/definitions/ShopLocation">>
        },
        <<"details">> => #{
          <<"$ref">> => <<"#/definitions/ShopDetails">>
        },
        <<"contractID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор договора, на основании которого производится обслуживание\nмагазина\n">>
        },
        <<"payoutToolID">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор средства вывода в рамках контракта, используемое в процессе\nвывода по магазину\n">>
        },
        <<"scheduleID">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int32">>,
          <<"description">> => <<"Идентификатор расписания выводов">>
        },
        <<"account">> => #{
          <<"$ref">> => <<"#/definitions/ShopAccount">>
        }
      },
      <<"description">> => <<"Данные магазина">>
    },
    <<"ShopAccount">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"currency">>, <<"guaranteeID">>, <<"settlementID">> ],
      <<"properties">> => #{
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"guaranteeID">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        },
        <<"settlementID">> => #{
          <<"type">> => <<"integer">>,
          <<"format">> => <<"int64">>
        }
      },
      <<"description">> => <<"Счета магазина">>
    },
    <<"ShopAmountResult">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"id">> ],
      <<"properties">> => #{
        <<"id">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор магазина">>
        },
        <<"amountResults">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/AmountResult">>
          }
        }
      },
      <<"description">> => <<"баланс по магазину\n">>,
      <<"example">> => #{
        <<"amountResults">> => [ <<"">>, <<"">> ],
        <<"id">> => <<"id">>
      }
    },
    <<"ShopDetails">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"name">> ],
      <<"properties">> => #{
        <<"name">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Название магазина">>,
          <<"maxLength">> => 100
        },
        <<"description">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Краткое описание">>,
          <<"maxLength">> => 1000
        }
      }
    },
    <<"ShopLocation">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"locationType">> ],
      <<"discriminator">> => <<"locationType">>,
      <<"properties">> => #{
        <<"locationType">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Тип местоположения">>,
          <<"enum">> => [ <<"ShopLocationUrl">> ]
        }
      },
      <<"description">> => <<"Местоположение магазина, по которому можно его найти">>,
      <<"x-discriminator-is-enum">> => true
    },
    <<"ShopLocationUrl">> => #{
      <<"allOf">> => [ #{
        <<"$ref">> => <<"#/definitions/ShopLocation">>
      }, #{
        <<"type">> => <<"object">>,
        <<"required">> => [ <<"url">> ],
        <<"properties">> => #{
          <<"url">> => #{
            <<"type">> => <<"string">>,
            <<"format">> => <<"uri">>,
            <<"description">> => <<"URL сайта магазина">>,
            <<"maxLength">> => 1000
          }
        }
      } ],
      <<"description">> => <<"Местоположение в Интернете">>
    },
    <<"SplitAmountResult">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"currency">>, <<"offsetAmounts">>, <<"splitUnit">> ],
      <<"properties">> => #{
        <<"splitUnit">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Единица времени сегмента разбиения">>,
          <<"enum">> => [ <<"minute">>, <<"hour">>, <<"day">>, <<"week">>, <<"month">>, <<"year">> ]
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"offsetAmounts">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/OffsetAmount">>
          }
        }
      },
      <<"example">> => #{
        <<"splitUnit">> => <<"minute">>,
        <<"currency">> => <<"currency">>,
        <<"offsetAmounts">> => [ #{
          <<"amount">> => 1,
          <<"offset">> => 0
        }, #{
          <<"amount">> => 1,
          <<"offset">> => 0
        } ]
      }
    },
    <<"SplitCountResult">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"currency">>, <<"splitUnit">>, <<"statusOffsetCounts">> ],
      <<"properties">> => #{
        <<"splitUnit">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Единица времени сегмента разбиения">>,
          <<"enum">> => [ <<"minute">>, <<"hour">>, <<"day">>, <<"week">>, <<"month">>, <<"year">> ]
        },
        <<"currency">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Валюта, символьный код согласно [ISO 4217](http://www.iso.org/iso/home/standards/currency_codes.htm).">>,
          <<"pattern">> => <<"^[A-Z]{3}$">>
        },
        <<"statusOffsetCounts">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/StatusOffsetCount">>
          }
        }
      },
      <<"example">> => #{
        <<"splitUnit">> => <<"minute">>,
        <<"statusOffsetCounts">> => [ #{
          <<"status">> => <<"pending">>,
          <<"offsetCount">> => [ <<"">>, <<"">> ]
        }, #{
          <<"status">> => <<"pending">>,
          <<"offsetCount">> => [ <<"">>, <<"">> ]
        } ],
        <<"currency">> => <<"currency">>
      }
    },
    <<"SplitUnit">> => #{
      <<"type">> => <<"string">>,
      <<"description">> => <<"Единица времени сегмента разбиения">>,
      <<"enum">> => [ <<"minute">>, <<"hour">>, <<"day">>, <<"week">>, <<"month">>, <<"year">> ]
    },
    <<"StatusOffsetCount">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"offsetCount">>, <<"status">> ],
      <<"properties">> => #{
        <<"status">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Статус платежа">>,
          <<"enum">> => [ <<"pending">>, <<"processed">>, <<"captured">>, <<"cancelled">>, <<"refunded">>, <<"failed">> ]
        },
        <<"offsetCount">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/OffsetCount">>
          }
        }
      },
      <<"example">> => #{
        <<"status">> => <<"pending">>,
        <<"offsetCount">> => [ <<"">>, <<"">> ]
      }
    },
    <<"SubError">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Детализация кода ошибки">>
        },
        <<"subError">> => #{
          <<"$ref">> => <<"#/definitions/SubError">>
        }
      },
      <<"description">> => <<"Детализация описания ошибки\n">>
    },
    <<"TransactionInfo">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"rrn">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Retrieval Reference Number">>
        },
        <<"approvalCode">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Authorization Approval Code">>
        }
      },
      <<"description">> => <<"Информация о транзакции">>
    },
    <<"inline_response_200">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/GroupByShopAmountResult">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ #{
          <<"groupBySHopResults">> => [ #{
            <<"amountResults">> => [ <<"">>, <<"">> ],
            <<"id">> => <<"id">>
          }, #{
            <<"amountResults">> => [ <<"">>, <<"">> ],
            <<"id">> => <<"id">>
          } ]
        }, #{
          <<"groupBySHopResults">> => [ #{
            <<"amountResults">> => [ <<"">>, <<"">> ],
            <<"id">> => <<"id">>
          }, #{
            <<"amountResults">> => [ <<"">>, <<"">> ],
            <<"id">> => <<"id">>
          } ]
        } ]
      }
    },
    <<"inline_response_200_1">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/AmountResult">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ]
      }
    },
    <<"inline_response_200_2">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/PaymentsToolDistributionResult">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ]
      }
    },
    <<"inline_response_200_3">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/CountResult">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ]
      }
    },
    <<"inline_response_200_4">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/PaymentsErrorsDistributionResult">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ]
      }
    },
    <<"inline_response_200_5">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/SplitAmountResult">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ #{
          <<"splitUnit">> => <<"minute">>,
          <<"currency">> => <<"currency">>,
          <<"offsetAmounts">> => [ #{
            <<"amount">> => 1,
            <<"offset">> => 0
          }, #{
            <<"amount">> => 1,
            <<"offset">> => 0
          } ]
        }, #{
          <<"splitUnit">> => <<"minute">>,
          <<"currency">> => <<"currency">>,
          <<"offsetAmounts">> => [ #{
            <<"amount">> => 1,
            <<"offset">> => 0
          }, #{
            <<"amount">> => 1,
            <<"offset">> => 0
          } ]
        } ]
      }
    },
    <<"inline_response_200_6">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/SplitCountResult">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ #{
          <<"splitUnit">> => <<"minute">>,
          <<"statusOffsetCounts">> => [ #{
            <<"status">> => <<"pending">>,
            <<"offsetCount">> => [ <<"">>, <<"">> ]
          }, #{
            <<"status">> => <<"pending">>,
            <<"offsetCount">> => [ <<"">>, <<"">> ]
          } ],
          <<"currency">> => <<"currency">>
        }, #{
          <<"splitUnit">> => <<"minute">>,
          <<"statusOffsetCounts">> => [ #{
            <<"status">> => <<"pending">>,
            <<"offsetCount">> => [ <<"">>, <<"">> ]
          }, #{
            <<"status">> => <<"pending">>,
            <<"offsetCount">> => [ <<"">>, <<"">> ]
          } ],
          <<"currency">> => <<"currency">>
        } ]
      }
    },
    <<"inline_response_200_7">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/PaymentsSubErrorsDistributionResult">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ]
      }
    },
    <<"inline_response_200_8">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"totalCount">> => #{
          <<"type">> => <<"integer">>
        },
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Chargeback">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ #{
          <<"chargebackId">> => <<"chargebackId">>,
          <<"levyAmount">> => 1,
          <<"providerFee">> => 0,
          <<"fee">> => 0,
          <<"levyCurrency">> => <<"levyCurrency">>,
          <<"externalId">> => <<"externalId">>,
          <<"bodyAmount">> => 1,
          <<"bodyCurrency">> => <<"bodyCurrency">>,
          <<"content">> => #{
            <<"data">> => <<"data">>,
            <<"type">> => <<"type">>
          },
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"stage">> => <<"chargeback">>,
          <<"paymentId">> => <<"paymentId">>,
          <<"chargebackReason">> => #{
            <<"code">> => <<"code">>,
            <<"category">> => #{ }
          },
          <<"invoiceId">> => <<"invoiceId">>,
          <<"shopID">> => <<"shopID">>,
          <<"externalFee">> => 0,
          <<"status">> => <<"pending">>
        }, #{
          <<"chargebackId">> => <<"chargebackId">>,
          <<"levyAmount">> => 1,
          <<"providerFee">> => 0,
          <<"fee">> => 0,
          <<"levyCurrency">> => <<"levyCurrency">>,
          <<"externalId">> => <<"externalId">>,
          <<"bodyAmount">> => 1,
          <<"bodyCurrency">> => <<"bodyCurrency">>,
          <<"content">> => #{
            <<"data">> => <<"data">>,
            <<"type">> => <<"type">>
          },
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"stage">> => <<"chargeback">>,
          <<"paymentId">> => <<"paymentId">>,
          <<"chargebackReason">> => #{
            <<"code">> => <<"code">>,
            <<"category">> => #{ }
          },
          <<"invoiceId">> => <<"invoiceId">>,
          <<"shopID">> => <<"shopID">>,
          <<"externalFee">> => 0,
          <<"status">> => <<"pending">>
        } ],
        <<"totalCount">> => 0,
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_9">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Invoice">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_10">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/PaymentSearchResult">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_11">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"totalCount">> => #{
          <<"type">> => <<"integer">>
        },
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Payout">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"amount">> => 1,
          <<"payoutToolDetails">> => #{
            <<"detailsType">> => <<"detailsType">>
          },
          <<"metadata">> => #{
            <<"payoutDesc">> => <<"Custom payout">>
          },
          <<"cancellationDetails">> => <<"cancellationDetails">>,
          <<"fee">> => 0,
          <<"currency">> => <<"currency">>,
          <<"id">> => <<"id">>,
          <<"shopID">> => <<"shopID">>,
          <<"status">> => <<"status">>
        }, #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"amount">> => 1,
          <<"payoutToolDetails">> => #{
            <<"detailsType">> => <<"detailsType">>
          },
          <<"metadata">> => #{
            <<"payoutDesc">> => <<"Custom payout">>
          },
          <<"cancellationDetails">> => <<"cancellationDetails">>,
          <<"fee">> => 0,
          <<"currency">> => <<"currency">>,
          <<"id">> => <<"id">>,
          <<"shopID">> => <<"shopID">>,
          <<"status">> => <<"status">>
        } ],
        <<"totalCount">> => 0,
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_12">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"totalCount">> => #{
          <<"type">> => <<"integer">>
        },
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/RefundSearchResult">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ <<"">>, <<"">> ],
        <<"totalCount">> => 0,
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_200_13">> => #{
      <<"type">> => <<"object">>,
      <<"properties">> => #{
        <<"continuationToken">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Токен, сигнализирующий о том, что в ответе передана только часть данных.\nДля получения следующей части нужно повторно обратиться к сервису, указав тот же набор условий и полученый токен.\nЕсли токена нет, получена последняя часть данных.\n">>
        },
        <<"result">> => #{
          <<"type">> => <<"array">>,
          <<"items">> => #{
            <<"$ref">> => <<"#/definitions/Report">>
          }
        }
      },
      <<"example">> => #{
        <<"result">> => [ #{
          <<"reportType">> => <<"provisionOfService">>,
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"fromTime">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"files">> => [ #{
            <<"filename">> => <<"filename">>,
            <<"id">> => <<"id">>,
            <<"signatures">> => #{
              <<"sha256">> => <<"sha256">>,
              <<"md5">> => <<"md5">>
            }
          }, #{
            <<"filename">> => <<"filename">>,
            <<"id">> => <<"id">>,
            <<"signatures">> => #{
              <<"sha256">> => <<"sha256">>,
              <<"md5">> => <<"md5">>
            }
          } ],
          <<"id">> => 0,
          <<"shopID">> => <<"shopID">>,
          <<"partyID">> => <<"partyID">>,
          <<"toTime">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"status">> => <<"pending">>
        }, #{
          <<"reportType">> => <<"provisionOfService">>,
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"fromTime">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"files">> => [ #{
            <<"filename">> => <<"filename">>,
            <<"id">> => <<"id">>,
            <<"signatures">> => #{
              <<"sha256">> => <<"sha256">>,
              <<"md5">> => <<"md5">>
            }
          }, #{
            <<"filename">> => <<"filename">>,
            <<"id">> => <<"id">>,
            <<"signatures">> => #{
              <<"sha256">> => <<"sha256">>,
              <<"md5">> => <<"md5">>
            }
          } ],
          <<"id">> => 0,
          <<"shopID">> => <<"shopID">>,
          <<"partyID">> => <<"partyID">>,
          <<"toTime">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"status">> => <<"pending">>
        } ],
        <<"continuationToken">> => <<"continuationToken">>
      }
    },
    <<"inline_response_400">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Код ошибки](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"ambiguousPartyID">>, <<"limitExceeded">>, <<"invalidRequest">>, <<"invalidPartyID">>, <<"invalidDeadline">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Max limit: 1000">>,
          <<"description">> => <<"Человекочитаемое описание ошибки">>
        }
      }
    },
    <<"inline_response_400_1">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"[Код ошибки](#tag/Error-Codes)\n">>,
          <<"enum">> => [ <<"ambiguousPartyID">>, <<"invalidShopID">>, <<"invalidRequest">>, <<"invalidDeadline">> ]
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"example">> => <<"Invalid shop id">>,
          <<"description">> => <<"Человекочитаемое описание ошибки">>
        }
      }
    },
    <<"FileMeta_signatures">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"md5">>, <<"sha256">> ],
      <<"properties">> => #{
        <<"md5">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"MD5 содержимого файла">>,
          <<"minLength">> => 32,
          <<"maxLength">> => 32
        },
        <<"sha256">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"SHA256 содержимого файла">>,
          <<"minLength">> => 64,
          <<"maxLength">> => 64
        }
      },
      <<"description">> => <<"Сигнатуры файла">>,
      <<"example">> => #{
        <<"sha256">> => <<"sha256">>,
        <<"md5">> => <<"md5">>
      }
    },
    <<"PaymentResource_clientInfo">> => #{
      <<"type">> => <<"object">>
    },
    <<"RefundStatus_error">> => #{
      <<"type">> => <<"object">>,
      <<"required">> => [ <<"code">>, <<"message">> ],
      <<"properties">> => #{
        <<"code">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Код ошибки, пригодный для обработки автоматическими системами">>
        },
        <<"message">> => #{
          <<"type">> => <<"string">>,
          <<"description">> => <<"Описание ошибки, пригодное для восприятия человеком">>
        }
      },
      <<"description">> => <<"Данные ошибки, возникшей в процессе проведения возврата, в случае если\nвозврат был неуспешен\n">>
    }
  },
  <<"parameters">> => #{
    <<"accountID">> => #{
      <<"name">> => <<"accountID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Account ID">>,
      <<"required">> => true,
      <<"type">> => <<"integer">>,
      <<"format">> => <<"int64">>
    },
    <<"requestID">> => #{
      <<"name">> => <<"X-Request-ID">>,
      <<"in">> => <<"header">>,
      <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 32,
      <<"minLength">> => 1
    },
    <<"shopIDQuery">> => #{
      <<"name">> => <<"shopID">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Идентификатор магазина">>,
      <<"required">> => false,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"shopIDPath">> => #{
      <<"name">> => <<"shopID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор магазина">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"partyIDQuery">> => #{
      <<"name">> => <<"partyID">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Уникальный идентификатор участника в рамках платформы">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"paymentID">> => #{
      <<"name">> => <<"paymentID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор платежа в рамках инвойса">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"refundID">> => #{
      <<"name">> => <<"refundID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор возврата в рамках платежа">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"contractID">> => #{
      <<"name">> => <<"contractID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор договора">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"residence">> => #{
      <<"name">> => <<"residence">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Резиденция, alpha-3 код по стандарту [ISO 3166-1](https://en.wikipedia.org/wiki/ISO_3166-1)">>,
      <<"required">> => false,
      <<"type">> => <<"string">>,
      <<"pattern">> => <<"^[A-Z]{3}$">>
    },
    <<"customerID">> => #{
      <<"name">> => <<"customerID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор кастомера">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"payoutID">> => #{
      <<"name">> => <<"payoutID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор вывода">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"invoiceID">> => #{
      <<"name">> => <<"invoiceID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор инвойса">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"fromTime">> => #{
      <<"name">> => <<"fromTime">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Начало временного отрезка">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"format">> => <<"date-time">>
    },
    <<"toTime">> => #{
      <<"name">> => <<"toTime">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Конец временного отрезка">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"format">> => <<"date-time">>
    },
    <<"splitUnit">> => #{
      <<"name">> => <<"splitUnit">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Единица времени сегмента разбиения">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"enum">> => [ <<"minute">>, <<"hour">>, <<"day">>, <<"week">>, <<"month">>, <<"year">> ]
    },
    <<"splitSize">> => #{
      <<"name">> => <<"splitSize">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Размер сегмента разбиения">>,
      <<"required">> => true,
      <<"type">> => <<"integer">>,
      <<"minimum">> => 1,
      <<"format">> => <<"int32">>
    },
    <<"limit">> => #{
      <<"name">> => <<"limit">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Лимит выборки">>,
      <<"required">> => true,
      <<"type">> => <<"integer">>,
      <<"maximum">> => 1000,
      <<"minimum">> => 1,
      <<"format">> => <<"int32">>
    },
    <<"offset">> => #{
      <<"name">> => <<"offset">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Смещение выборки">>,
      <<"required">> => false,
      <<"type">> => <<"integer">>,
      <<"minimum">> => 0
    },
    <<"deadline">> => #{
      <<"name">> => <<"X-Request-Deadline">>,
      <<"in">> => <<"header">>,
      <<"description">> => <<"Максимальное время обработки запроса">>,
      <<"required">> => false,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"shopIDs">> => #{
      <<"name">> => <<"shopIDs">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Идентификаторы магазинов">>,
      <<"required">> => false,
      <<"type">> => <<"array">>,
      <<"items">> => #{
        <<"type">> => <<"string">>
      }
    },
    <<"excludeShopIDs">> => #{
      <<"name">> => <<"excludeShopIDs">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Идентификаторы магазинов не включаемые в запрос">>,
      <<"required">> => false,
      <<"type">> => <<"array">>,
      <<"items">> => #{
        <<"type">> => <<"string">>
      }
    },
    <<"reportType">> => #{
      <<"name">> => <<"reportType">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Тип отчета">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"enum">> => [ <<"provisionOfService">>, <<"paymentRegistry">> ]
    },
    <<"reportID">> => #{
      <<"name">> => <<"reportID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор отчета">>,
      <<"required">> => true,
      <<"type">> => <<"integer">>,
      <<"format">> => <<"int64">>
    },
    <<"fileID">> => #{
      <<"name">> => <<"fileID">>,
      <<"in">> => <<"path">>,
      <<"description">> => <<"Идентификатор файла">>,
      <<"required">> => true,
      <<"type">> => <<"string">>,
      <<"maxLength">> => 40,
      <<"minLength">> => 1
    },
    <<"paymentInstitutionRealm">> => #{
      <<"name">> => <<"paymentInstitutionRealm">>,
      <<"in">> => <<"query">>,
      <<"description">> => <<"Тип магазина, тестовый или «боевой»">>,
      <<"required">> => false,
      <<"type">> => <<"string">>,
      <<"enum">> => [ <<"live">>, <<"test">> ]
    }
  },
  <<"responses">> => #{
    <<"NotFound">> => #{
      <<"description">> => <<"Заданный ресурс не найден">>,
      <<"schema">> => #{
        <<"$ref">> => <<"#/definitions/GeneralError">>
      }
    },
    <<"Unauthorized">> => #{
      <<"description">> => <<"Ошибка авторизации">>
    },
    <<"DefaultLogicError">> => #{
      <<"description">> => <<"Неверные данные">>,
      <<"schema">> => #{
        <<"$ref">> => <<"#/definitions/DefaultLogicError">>
      }
    }
  }
}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(SCHEMA,
  <<"{\"definitions\": {
       \"Pet\": {
         \"type\":          \"object\",
         \"discriminator\": \"petType\",
         \"properties\": {
            \"name\":    {\"type\": \"string\"},
            \"petType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"petType\"]
       },
       \"Cat\": {
         \"description\": \"A representation of a cat\",
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"huntingSkill\": {
                 \"type\":        \"string\",
                 \"description\": \"The measured skill for hunting\",
                 \"default\":     \"lazy\",
                 \"enum\":        [\"clueless\", \"lazy\", \"adventurous\", \"aggressive\"]
               }
             },
             \"required\": [\"huntingSkill\"]
           }
         ]
       },
       \"Dog\": {
         \"description\": \"A representation of a dog\",
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"packSize\": {
                 \"type\":        \"integer\",
                 \"format\":      \"int32\",
                 \"description\": \"the size of the pack the dog is from\",
                 \"default\":     0,
                 \"minimum\":     0
               }
             }
           }
         ],
         \"required\": [\"packSize\"]
       },
       \"Person\": {
         \"type\":          \"object\",
         \"discriminator\": \"personType\",
         \"properties\": {
           \"name\": {\"type\": \"string\"},
           \"sex\": {
             \"type\": \"string\",
             \"enum\": [\"male\", \"female\"]
           },
           \"personType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"sex\", \"personType\"]
       },
       \"WildMix\": {
         \"allOf\": [
           {\"$ref\": \"#/definitions/Pet\"},
           {\"$ref\": \"#/definitions/Person\"}
         ],
       },
       \"Dummy\": {
         \"type\":          \"object\",
         \"discriminator\": \"dummyType\",
         \"properties\": {
           \"name\":      {\"type\": \"string\"},
           \"dummyType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"dummyType\"]
       }
     }}">>).

get_enum(Parent, Discr, Schema) ->
    lists:sort(deep_get([?DEFINITIONS, Parent, <<"properties">>, Discr, <<"enum">>], Schema)).

deep_get([K], M) ->
    maps:get(K, M);
deep_get([K | Ks], M) ->
    deep_get(Ks, maps:get(K, M)).

-spec test() -> _.
-spec enumerate_discriminator_children_test() -> _.
enumerate_discriminator_children_test() ->
    Schema      = jsx:decode(?SCHEMA, [return_maps]),
    FixedSchema = enumerate_discriminator_children(Schema),
    ?assertEqual(lists:sort([<<"Dog">>, <<"Cat">>, <<"WildMix">>]), get_enum(<<"Pet">>, <<"petType">>, FixedSchema)),
    ?assertEqual([<<"WildMix">>], get_enum(<<"Person">>,  <<"personType">>, FixedSchema)),
    ?assertEqual([],              get_enum(<<"Dummy">>,   <<"dummyType">>,  FixedSchema)).

-spec get_test() -> _.
get_test() ->
    ?assertEqual(
       enumerate_discriminator_children(maps:with([?DEFINITIONS], get_raw())),
       ?MODULE:get()
    ).
-endif.
