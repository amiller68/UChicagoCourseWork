-- Jerome helped me get an idea of how I could tackle the problem
data Person
  = Student
      { firstName :: String
      , lastName :: String
      , id :: String
      , major :: String
      , year :: Int
      , courses_enrolled :: [(String, (Int, Int))]
      }
  | Teacher
      { firstName :: String
      , lastName :: String
      , dept :: String
      , courses_teaching :: [(Int, Int)]
      }
      deriving (Show)

------------------------------------------------------------------------------

studentsOfTeacher_ :: [Person] -> Person -> [((Int, Int), [(String, String)])]
studentsOfTeacher_ students teacher = formatRoster students department courses
--  | null courses = []
--  | (null (getStudents students (department, head courses)) == False)
--    = (head courses, getStudents students (department, head courses)) :
--  | otherwise = (formatRoster students (head courses)) : (formatRoster students (tail courses))
  where (courses, department) = (courses_teaching teacher, dept teacher)


formatRoster :: [Person] -> String -> [(Int, Int)] -> [((Int, Int), [(String, String)])]
formatRoster students department courses
  | null courses = []

--  | null (getStudents students (department, course)) = (course, [("NO STUDENTS", "sOrry")])
  | otherwise = ((head courses), getStudents students (department, (head courses))) : (formatRoster students department (tail courses))


--map (, (1,2)) [formatName students]

  --let listStudents = filter (==department teacher) students
--  let dept = department teacher
  --let teaching = [(t dept, ())
  --let taking = courses_enrolled student in
--  all (==(dept, course)) taking


studentsOfTeacher = studentsOfTeacher_ allStudents
{-}
isStudent :: courses_enrolled Person -> Person -> Bool
isStudent [] teacher = False
isStudent course:courses teacher
  | course == (dept teacher, )
-}

--courselist :: Person -> (a -> [b]) -> [(a,b)]
--courselist c = define_courses (dept c) (courses_teaching c)
{-}
define_courses :: a -> [b] -> [(a,b)]
define_courses _ [] = []
define_courses dept (section:sections) = (,) dept section : (define_courses dept sections)
-}
getStudents :: [Person] -> (String, (Int, Int)) -> [(String, String)]
getStudents students course
  | null students = []
  | (null student_courses) = []
  | course `elem`  student_courses = formatName (head students) : getStudents (tail students) course
  | otherwise = getStudents (tail students) course
  where student_courses = courses_enrolled (head students)

getTeacherCourses :: Person -> [(String, (Int, Int))]
getTeacherCourses teacher = map ((,) department) teacher_courses
  where (teacher_courses, department) = (courses_teaching teacher, dept teacher)


formatName :: Person -> (String, String)
formatName person = (,) (lastName person) (firstName person)

formatNames :: [Person] -> [(String, String)]
formatNames persons = map formatName persons
------------------------------------------------------------------------------

professorChugh =
  Teacher "Ravi" "Chugh" "CMSC" [(16100,1)]

professorKurtz =
  Teacher "Stuart" "Kurtz" "CMSC" [(16100,2), (28000,1)]

allStudents =
  [ Student "A" "Student" "********" "CMSC" 1 [("CMSC", (15100,1))]
  , Student "B" "Student" "********" "CMSC" 1 [("CMSC", (16100,1))]
  , Student "C" "Student" "********" "CMSC" 2 [("CMSC", (16100,2))]
  , Student "D" "Student" "********" "MATH" 2 [("CMSC", (28000,1))]
  , Student "E" "Student" "********" "MATH" 3 [("CMSC", (28000,1))]
  , Student "F" "Student" "********" "ARTV" 3 [("CMSC", (12100,1))]
  , Student "STEAM" "Student" "********" "ARTV" 4
      [("CMSC", (16100,1)), ("ARTV", (22500,1)), ("ARTV", (22502,1))]
  ]
