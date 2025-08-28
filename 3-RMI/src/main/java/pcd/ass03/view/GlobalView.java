package pcd.ass03.view;

import pcd.ass03.model.GameStateManager;

import javax.swing.*;
import java.awt.*;

public class GlobalView extends JFrame {

    private final GamePanel gamePanel;

    public GlobalView(GameStateManager gameStateManager) {
        setTitle("Agar.io - Global View (Java)");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // Or DISPOSE_ON_CLOSE if multiple windows
        setPreferredSize(new Dimension(800, 800));

        this.gamePanel = new GamePanel(gameStateManager);
        add(this.gamePanel, BorderLayout.CENTER);

        pack();
        setLocationRelativeTo(null);
    }

    public void repaintView() {
        if (gamePanel != null) {
            gamePanel.repaint();
        }
    }
}
