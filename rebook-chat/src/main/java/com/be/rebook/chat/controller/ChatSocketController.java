package com.be.rebook.chat.controller;

import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.simp.SimpMessageSendingOperations;
import org.springframework.stereotype.Controller;

import com.be.rebook.chat.dto.ChatMessage;
import com.be.rebook.chat.repository.ChatRoomRepository;
import com.be.rebook.chat.service.RedisPublisher;

import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Controller
public class ChatSocketController {

    private final SimpMessageSendingOperations messagingTemplate;

    private final RedisPublisher redisPublisher;
    private final ChatRoomRepository chatRoomRepository;

    /**
     * websocket "/pub/chat/message"로 들어오는 메시징을 처리한다.
     */
    @MessageMapping("/chat/message")
    public void message(ChatMessage message) {
        System.out.println("message: " + message);
        if (ChatMessage.MessageType.ENTER.equals(message.getType())) {
            chatRoomRepository.enterChatRoom(message.getRoomId());
            message.setMessage(message.getSenderId() + "님이 입장하셨습니다.");
        }
        // Websocket에 발행된 메시지를 redis로 발행한다(publish)
        redisPublisher.publish(chatRoomRepository.getTopic(message.getRoomId()), message);
    }

}