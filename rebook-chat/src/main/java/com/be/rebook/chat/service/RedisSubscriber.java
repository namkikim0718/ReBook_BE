package com.be.rebook.chat.service;

import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import com.be.rebook.chat.dto.ChatMessage;

@Service
public class RedisSubscriber {

    private final SimpMessagingTemplate messagingTemplate;

    public RedisSubscriber(SimpMessagingTemplate messagingTemplate) {
        this.messagingTemplate = messagingTemplate;
    }

    public void handleMessage(ChatMessage message) {
        // 메시지를 해당 roomId에 브로드캐스트
        messagingTemplate.convertAndSend("/topic/chat/" + message.getRoomId(), message);
    }
}
