package main

/*
 * @author Bedeschi Federica   federica.bedeschi4@studio.unibo.it
 * @author Pracucci Filippo    filippo.pracucci@studio.unibo.it
 */

import (
	"fmt"
	"math/rand"
	"time"
)

type Move int

const (
	Paper Move = iota
	Scissors
	Rock
)

var moveName = map[Move]string{
	Paper:    "paper",
	Scissors: "scissors",
	Rock:     "rock",
}

type Result int

const (
	Win Result = iota
	Lose
	Draw
)

var resultName = map[Result]string{
	Win:  "win",
	Lose: "lose",
	Draw: "draw",
}

var result = map[[2]Move]Result{
	{Rock, Scissors}:     Win,
	{Scissors, Paper}:    Win,
	{Paper, Rock}:        Win,
	{Scissors, Rock}:     Lose,
	{Paper, Scissors}:    Lose,
	{Rock, Paper}:        Lose,
	{Rock, Rock}:         Draw,
	{Scissors, Scissors}: Draw,
	{Paper, Paper}:       Draw,
}

func main() {
	moveChannel1 := make(chan Move)
	moveChannel2 := make(chan Move)
	resultChannel1 := make(chan Result)
	resultChannel2 := make(chan Result)
	go referee(moveChannel1, moveChannel2, resultChannel1, resultChannel2)
	go player(1, moveChannel1, resultChannel1)
	go player(2, moveChannel2, resultChannel2)

	for {
	}
}

func referee(moveChannel1 chan Move, moveChannel2 chan Move, resultChannel1 chan Result,
	resultChannel2 chan Result) {
	for {
		moveP1, moveP2 := <-moveChannel1, <-moveChannel2
		resultChannel1 <- result[[2]Move{moveP1, moveP2}]
		resultChannel2 <- result[[2]Move{moveP2, moveP1}]
		time.Sleep(1 * time.Second)
		fmt.Println()
	}
}

func player(id uint, moveChannel chan Move, resultChannel chan Result) {
	score := 0
	for {
		move := Move(rand.Intn(3))
		moveChannel <- move
		result := <-resultChannel
		if result == Win {
			score++
		}
		fmt.Printf("Player %d: %-8s => %-4s \tScore = %d\n", id, moveName[move], resultName[result], score)
	}
}
