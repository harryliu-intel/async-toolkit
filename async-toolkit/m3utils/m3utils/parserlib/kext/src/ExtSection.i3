INTERFACE ExtSection;
TYPE
  T = {Pragma, Interface, Proc, Module};
PROCEDURE GetText(kind: CHAR; i: T): TEXT;
PROCEDURE Res(name: TEXT): TEXT;
END ExtSection.
