package com.be.rebook.chat.dto;

import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class CreateChatRoomDto {

    @NotNull(message = "Buyer id is required")
    private Long buyerId;

    @NotNull(message = "Seller id is required")
    private Long sellerId;

    @NotNull(message = "Product id is required")
    private Long productId;
}
