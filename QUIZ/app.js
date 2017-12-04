function populate() {
    if(quiz.isEnded()) {
        showScores();
    } else {
        // show question
        var element = document.getElementById('question');
        element.innerHTML = quiz.getQuestionIndex().text;
 
 
   
        var choices = quiz.getQuestionIndex().choices;
       
       
 
       
        for (var i= 0; i < choices.length; i++) {
            var element = document.getElementById('choice' + i);//choice 0
            element.innerHTML = choices[i]; //set choice 0 innerhtml to slytherin
            guess("btn" + i, choices[i]);
 
        }
 
        showProgress();
    }
}
 
function guess(id, guess) {
    var button = document.getElementById(id);
    button.onclick = function() {
        quiz.guess(guess);
        populate();
    }
}
 
 
function showProgress() {
    var currentQuestionNumber = quiz.questionIndex + 1;
    var element = document.getElementById("progress");
    element.innerHTML = "Question " + currentQuestionNumber + " of " + quiz.questions.length;
 
}
 
 
 
function showScores() {
    var gameOverHtml = "<h1>Result</h1>";
    gameOverHtml += "<h2 id='score'>Score: " + quiz.score + "</h2>";
    var element = document.getElementById("quiz");
    element.innerHTML = gameOverHtml;
 
}
 
 
 
 
var questions = [
    new Question("What is harrys house?", ["Slytherin", "HufflePuff", "Gryffindor", "Ravenclaw"], "Gryffindor"),
    new Question("What is Dracos house?", ["Slytherin", "HufflePuff", "Gryffindor", "Ravenclaw"], "Slytherin"),
    new Question("What is hermoines house?", ["Slytherin", "HufflePuff", "Gryffindor", "Ravenclaw"], "Gryffindor"),
    new Question("What is rons house?", ["Slytherin", "HufflePuff", "Gryffindor", "Ravenclaw"], "Gryffindor"),
    new Question("What is nevilles house?", ["Slytherin", "HufflePuff", "Gryffindor", "Ravenclaw"], "Gryffindor")
];
 
 
 
 
 
 

/*

var questions = [
    new Question ("Ai là người sáng lập Apple?", ["Bill Gates", "Steve Jobs", "Lary Pages", "Donal Trump"], "Steve Jobs"),
    new Question ("Việt Nam gia nhập liên Hợp Quốc vào năm nào?", ["1977", "1978", "1987", "1988"], "1977"),
    new Question ("Quốc gia nào sẽ tổ chức Worldcup 2018?", ["Bỉ", "Brazin", "Nga", "Ả rập xê út"], "Nga"),
    new Question ("Chủ tịch Hồ Chí Minh sinh năm bao nhiêu", ["1890", "1895", "1900", "1905"], "1890"),
    new Question ("Ai là tác giả của bài hát 'Em gái mưa'?", ["Văn Mai Hương", "Chi Pu", "Mr Siro", "Đỗ Hiếu"], "Đỗ Hiếu"),
    new Question ("Messi đã giành bao nhiêu quả bóng vàng FIFA", ["3", "4", "5", "6"], "5"),
];

*/

//slot in the quiz object instance

var quiz = new Quiz(questions);

populate();