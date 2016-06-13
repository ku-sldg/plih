---
layout: frontpage
title: EECS 662
---

# EECS 662 - Programming Languages

Programming Languages is an introduction to basic principles of
defining, describing and implementing interpreters for programming languages. The fundamental goal is establishing a vocabulary for
discussing what programming languages and programs written in them do. Topics covered to accomplish this are data representation and types; declarations, bindings and variable assignment; parameter passing and function evaluation; statements; and objects and types;  The course uses an implementation-based approach with students developing type checkers, interpreters for languages that demonstrate features presented in class. 

# Class Information 

----

Room and Time
: {{ site.where }}
: {{ site.when }}

Prerequisites
: EECS 368 is a hard prerequisite.

Instructor Information
: [Dr. Perry Alexander](http://palexand.github.io)
: Office Hours: {{ site.ohrs }} in 2022 Eaton Hall, or by appointment
: Office: 2022 Eaton Hall / 208 Nichols Hall 
: Phone: 4-8833 / 4-7741 
: Web: [http://www.ittc.ku.edu/~alex](http://www.ittc.ku.edu/~alex)
: Email: <palexand@ku.edu>

Textbooks
: [Shriram Krishnamurthi](http://www.cs.brown.edu/people/sk/),
  *Programming Languages: Application and Interpretation*,
  [published online](http://www.cs.brown.edu/people/sk/Publications/Books/ProgLangs/2007-04-26/). (Required)
:  
: Matthias Felleisen, Robert Bruce Findler, Matthew Flatt, Shriram
  Krishnamurthi, *How To Design Programs: An Introduction to Computing
  and Programming*, The MIT Press, 2001,
  [published online](http://www.htdp.org/2003-09-26/Book/curriculum.html).
  (Scheme Reference) 

# Mini-Projects

----

You will perform several projects that involve developing code using the Racket programming language. These projects are an exceptionally important part of the course and provide insight into building interpreters that cannot be gained from lectures or exams. Programming projects will be assigned every 1-2 weeks.  The PLT Racket interpreter, MzRacket, and the DrRacket development environment will be available on the EECS Department's Linux systems. Interpreters for virtually any platform you might have are also available for download from the PLT website.  I strongly recommend using DrRacket for your assignments. 

You are required to submit your documented source code, testing
results, test input files you have prepared, and any documentation needed to execute your code. Use tar to create an archive of your
submission. Please use KU's Blackboard system for submissions.  Note that this and reporting grades are the only Blackboard functions we will use.  I will not accept paper listings when electronic submission is required. Your programs will be extracted, compiled and tested in the Linux environment. Please document your code source files making certain to include your name and ID number. Undocumented source code will result in a serious point deduction!

### Project Descriptions

<ul>
{% for post in site.categories.project reversed %}
<li><a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>
 - Due: {{ post.when }} 11:59pm</li>
{% endfor %}
</ul>

# Exams

----

All exams are close book, closed notes, in-class exams.  The final exam will be held during the time assigned by the University in our regular classroom.

<ul>
{% for post in site.categories.exam reversed %}
<li><a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>
 - Date: {{ post.when }}</li>
{% endfor %}
</ul>

# Classroom Policies

----

#### The Three Commandments

Pretty simple.  Follow them and we'll be great friends!

1. Don't whine
1. Don't cheat
1. Don't tell me what you don't need to know

#### Class Participation

I do not take attendance in class; however participation in class is important to your success, however participation in class is important to its success. How much homework and how rigorously it is graded will definitely depend on class participation. Please ask questions and participate in class discussions. When assigning final grades, borderline cases will be decided based on class participation.  

#### Grading Errors

If I have made an error in grading an exam or assignment, you have two weeks following the date the item is available to see me about correcting the problem. Note that this includes the final! After that time, your grade is set and will not be changed. I also request that you wait 24 hours after an exam is returned before coming to me with questions.  

#### Curving

I may decide to curve final scores when the quarter is over. Whether I curve and how much I curve is at my discretion. I will never curve up, but may curve down. Specifically, 90% and above will always be an "A", but I may choose to lower the cutoff percentage. Whether I curve and how much I curve is at my discretion. I will never curve scores on an individual graded assignment, lab or exam.  

#### Email

I encourage you to use email to contact me. I am logged in whenever I am working and check my mail frequently. Email is my preferred means of communication.  

#### Blog

The course blog is available on the [website](blog.html). I will post late-breaking news about projects, homework and class administration on the blog. Check the website and blog freqently, particularly around project due dates and exams.  

#### Phone

Feel free to call me in any of my office at any time. I would prefer not to be called at home.  

#### Office Hours

I will make every effort to be in my office during scheduled office hours. If there are exceptions, I will let you know as early as is possible. If you have a conflict with my office hours, please make an appointment. I have an open door policy, you are free to come by whenever you choose. If I am busy, I may ask that you come back later, but please don't hesitate to knock! My schedule is available online.  

#### Cheating

Academic misconduct of any kind will automatically result in a 0 score on the homework, lab, project, or exam in question and your actions will be reported to the department chair. Your homework, exams and projects must be individually prepared unless otherwise noted. Posting your assignments to internet discussion lists is considered academic misconduct. Sharing your solutions with others is considered academic misconduct. Turning in solutions from previous semesters is considered academic misconduct. Paying people to prepare solutions is academic misconduct. Automated mechanisms are available for checking the originality of source code. Please spend your time trying to solve assigned problems rather than trying to get around the system. Don't risk it!  

#### Excuses

Excusing a missed exam or assignment is left to the discretion of the instructor. Illness, family emergencies, and religious observances are examples of acceptable excuses. Computer down time, over sleeping, and social events are examples of unacceptable excuses. Please try to let me know of problems in advance when possible and be prepared to provide verification of your excuse.  

#### Extensions

As a policy, I do not extend due dates of homework and projects. If I choose to do so, I will only announce the extension in class, via email or on the blog. If you hear an extension has been granted and I have not announced it, your information is incorrect. Remember that if I grant extensions early in the semester, it will necessarily compress due dates the end of the semester. 

#### Dress Code

Ties are expressly forbidden in my classroom.  If you wear one you will be taunted mercilessly.  Exceptions are made for bow ties which we all know are cool.

# Grading 

----

Grades are assigned on a standard 10 point scale:

* A = 90-100%
* B = 80-90%
* C = 70-80%
* D = 60-70%
* F = 0-60%

Classroom tasks are weighted using the following scale

* Midterm Exam 30%
* Final Exam 30%
* Homework and Mini Projects 40%

You must pass both projects and homework, and exams separately to pass the class.  Specifically, if you get less than 60\% on your projects and homework or less than 60\% on your exams, you will not pass the course.

I may curve final grades at the end of the semester.  However, I will never curve individual assignments or exams.  If I curve and how much I curve is at my discretion.

# Topics

----

We will cover topics in roughly the same order as our text.  I will also add miscellaneous topics throughout the semester.  Specific topics are subject to change without notice and topics marked "(tentative)" will be covered as time permits. 

* Introduction
* Modeling Languages
* Substitution and Functions
* Laziness
* Recursion
* State
* Continuations
* Memory Management
* Semantics
* Types and Type Checking
* Programming by Searching (tentative)
* Domain-Specific Languages
* Metaprogramming (tentative)
