package com.be.rebook.chat.dto;

import com.be.rebook.chat.entity.ChatRoom;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class ChatRoomDto {

    private Long roomId;

    private Long buyerId;

    private Long sellerId;

    private Long productId;

    public ChatRoomDto(ChatRoom chatRoom) {
        this.roomId = chatRoom.getId();
        this.buyerId = chatRoom.getBuyerId();
        this.sellerId = chatRoom.getSellerId();
        this.productId = chatRoom.getProductId();
    }

    public String toString() {
        return "ChatRoomDto{" +
                "roomId=" + roomId +
                ", buyerId=" + buyerId +
                ", sellerId=" + sellerId +
                ", productId=" + productId +
                '}';
    }
}
