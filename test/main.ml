open! Core_kernel

let b =
  Bactrian.setup
    [
      ("factors", [ "none"; "one"; "multiple" ]);
      ("test cases", [ "missing"; "enough" ]);
      ("factor recognized", [ "yes"; "no" ]);
      ("value recognized", [ "yes"; "no" ]);
    ]

let bt = Bactrian.named_test b

let test_missing_one () =
  let uut = Bactrian.setup [ ("test factor", [ "a"; "b" ]) ] in
  Bactrian.test uut [ ("test factor", "a") ];
  Alcotest.(check (result unit string))
    "should fail"
    (Result.fail "Missing tests:\n* test factor=b")
    (Bactrian.finish uut)

let test_enough () =
  let uut = Bactrian.setup [ ("test factor", [ "a"; "b" ]) ] in
  Bactrian.test uut [ ("test factor", "a") ];
  Bactrian.test uut [ ("test factor", "b") ];
  Alcotest.(check (result unit string))
    "should be ok" (Result.return ()) (Bactrian.finish uut)

let test_none () =
  let uut = Bactrian.setup [] in
  Bactrian.test uut [];
  Bactrian.test uut [];
  Alcotest.(check (result unit string))
    "should be ok" (Result.return ()) (Bactrian.finish uut)

let test_unrecognized =
  ( bt
      [
        ("factors", "one"); ("test cases", "enough"); ("factor recognized", "no");
      ],
    `Quick,
    fun () ->
      let uut = Bactrian.setup [ ("test factor", [ "a"; "b" ]) ] in
      Bactrian.test uut [ ("test factor", "a") ];
      Bactrian.test uut [ ("test factor", "b") ];
      Bactrian.test uut [ ("another factor", "a") ];
      Alcotest.(check (result unit string))
        "should fail"
        (Result.fail "Unknown factors: another factor")
        (Bactrian.finish uut) )

let suite =
  [
    ( bt [ ("factors", "one"); ("test cases", "missing") ],
      `Quick,
      test_missing_one );
    (bt [ ("factors", "one"); ("test cases", "enough") ], `Quick, test_enough);
    (bt [ ("factors", "none"); ("test cases", "enough") ], `Quick, test_none);
    test_unrecognized;
    ("Bactrian", `Quick, Bactrian.alcotest b);
  ]

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  Alcotest.run "bactrian" [ ("suite", suite) ]
