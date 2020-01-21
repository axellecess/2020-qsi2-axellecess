open Framework;
open QCheckRely;
open Generator.Fantasy;
open Lib.Troll;

let {describe} = extendDescribe(QCheckRely.Matchers.matchers);

describe("Troll Invariance", ({test}) => {
  test("Troll score should be 0 when all elves resurrected", ({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="Troll score should be 0 when all elves resurrected",
      troll_arbitrary,
      troll =>
      all_elves_resurrected(troll) |> scoring == 0
    )
    |> expect.ext.qCheckTest;
    ();
  });
  test("Troll score should always be >= 0", ({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="Troll score should always be >= 0",
      troll_arbitrary,
      troll =>
      scoring(troll) >= 0
    )
    |> expect.ext.qCheckTest;
    ();
  });
});

describe("Troll Inverse", ({test}) => {
  test("oops_he_survived should always be inverse of i_got_one", ({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="oops_he_survived should always be inverse of i_got_one",
      troll_elf_arbitrary,
      ((troll, elf)) =>
      troll |> i_got_one(elf) |> oops_he_survived(elf) |> scoring == scoring(troll)
    )
    |> expect.ext.qCheckTest;
    ();
  })
});

describe("Troll Analogy", ({test}) => {
  test("i_got_one and i_got should be consistent", ({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="i_got_one and i_got should be consistent",
      troll_elf_int_arbitrary,
      ((troll, elf, num)) =>{
        let trollManyGot = ref(troll);
        for(i in 1 to num){
          trollManyGot := i_got_one(elf, trollManyGot^);
        };
      (trollManyGot^ |> scoring) == (troll |> i_got(num, elf) |> scoring)
    }
    )
    |> expect.ext.qCheckTest;
    ();
  })
});

describe("Troll Idempotence", ({test}) => {
  test("all_elves_of_a_kind_resurrected brings the Troll killing list to a stable state",({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="all_elves_of_a_kind_resurrected brings the Troll killing list to a stable state",
      troll_elf_arbitrary,
      ((troll, elf)) =>
      {
        let trollGotOneElf = all_elves_of_a_kind_resurrected(elf, i_got_one(elf, troll));

        trollGotOneElf == all_elves_of_a_kind_resurrected(elf, trollGotOneElf);
      }
    )
    |> expect.ext.qCheckTest;
    ();
  })
});

describe("Troll Metamorphism", ({test}) => {
  test("i_got_one increase number of kill and the score",({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="i_got_one increase number of kill and the score",
      troll_elf_arbitrary,
      ((troll, elf)) =>
      {
        let trollAfter = i_got_one(elf, troll);

        scoring(troll) <= scoring(trollAfter);
      }
    )
    |> expect.ext.qCheckTest;
    ();
  })
});

describe("Troll Injection", ({test}) => {
  test("",({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="",
      troll_two_elves_arbitrary,
      ((troll, elf_One, elf_Two)) =>
      {

        let trollAfterElfOne = i_got_one(elf_One, troll);
        let trollAfterElfTwo = i_got_one(elf_Two, troll);
        if(elf_One == elf_Two){
          trollAfterElfOne == trollAfterElfTwo;
        }
        else {
          trollAfterElfOne != trollAfterElfTwo;
        }
      }
    )
    |> expect.ext.qCheckTest;
    ();
  })
});