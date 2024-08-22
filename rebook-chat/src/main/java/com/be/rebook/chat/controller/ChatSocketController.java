package com.be.rebook.chat.controller;

import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;

import com.be.rebook.chat.dto.ChatMessage;
import com.be.rebook.chat.service.ChatService;
import com.be.rebook.chat.service.RedisPublisher;

import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Controller
public class ChatSocketController {

    private final ChatService chatMessageService;

    @MessageMapping("/chat/message")
    public void message(ChatMessage chatMessage) {
        // User user =
        // userRepository.findById(chatMessageRequest.getUserId()).orElseThrow(UserNotFoundException::new);

        chatMessageService.sendMessage(chatMessage);
    }
}
