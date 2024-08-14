package com.be.rebook.chat.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class ChatMessage {
    private Long senderId;
    private String content;
    private Long roomId; // 특정 채팅방을 구분하는 ID

    @Override
    public String toString() {
        return "ChatMessage{" +
                "senderId='" + senderId + '\'' +
                ", content='" + content + '\'' +
                ", roomId='" + roomId + '\'' +
                '}';
    }
}
