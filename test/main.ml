open! Core_kernel

let b =
  Bactrian.setup
    [
      ("factors", [ "none"; "one"; "multiple" ]);
      ("test cases", [ "missing"; "enough" ]);
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
  Bactrian.test uut [ ("test factor", "a") ];
  Bactrian.test uut [ ("test factor", "b") ];
  Alcotest.(check (result unit string))
    "should be ok" (Result.return ()) (Bactrian.finish uut)

let suite =
  [
    ( bt [ ("factors", "one"); ("test cases", "missing") ],
      `Quick,
      test_missing_one );
    (bt [ ("factors", "one"); ("test cases", "enough") ], `Quick, test_enough);
    (bt [ ("factors", "none"); ("test cases", "enough") ], `Quick, test_none);
    ("Bactrian", `Quick, Bactrian.alcotest b);
  ]

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  Alcotest.run "bactrian" [ ("suite", suite) ]
