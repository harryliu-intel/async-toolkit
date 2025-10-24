.global _RESET;

define TEST(y) {
  gma {
    _RESET==0 -> y(0)=1, y(1)=0, y(2)=0

    _RESET==1 && y(0) -> instant y(0)=0, after 100 y(1)=1,
                         instant stdout="Time is now " ++ DEC(TIME())
    _RESET==1 && y(1) -> instant y(1)=0, after 100 y(2)=1,
                         instant stdout="Time is now " ++ DEC(TIME())
    _RESET==1 && y(2) -> instant y(2)=0, after 100 y(0)=1,
                         instant stdout="Time is now " ++ DEC(TIME())
  }
}

group x(a,b,c);

TEST test(x);
